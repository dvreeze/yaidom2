/*
 * Copyright 2019-2019 Chris de Vreeze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eu.cdevreeze.yaidom2.node.saxon

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream

import net.sf.saxon.s9api.Serializer

import scala.io.Codec

/**
 * Saxon document serializer.
 *
 * @author Chris de Vreeze
 */
object SaxonSerializer {

  /**
   * Serializes the given SaxonDocument.
   */
  def serialize(saxonDoc: SaxonDocument, file: File, configureSerializer: Serializer => Unit): Unit = {
    serialize(saxonDoc, new FileOutputStream(file), configureSerializer)
  }

  /**
   * Serializes the given SaxonDocument. This method closes the OutputStream at the end.
   */
  def serialize(saxonDoc: SaxonDocument, os: OutputStream, configureSerializer: Serializer => Unit): Unit = {
    val serializer: Serializer = saxonDoc.newSerializer(os)
    configureSerializer(serializer)
    serializer.serializeNode(saxonDoc.xdmNode)
    serializer.close()
    os.close()
  }

  /**
   * Serializes the given SaxonDocument, adding no indentation by itself. Encoding UTF-8 is used.
   * This method closes the OutputStream at the end.
   */
  def serialize(saxonDoc: SaxonDocument, file: File): Unit = {
    serialize(saxonDoc, new FileOutputStream(file))
  }

  /**
   * Serializes the given SaxonDocument, adding no indentation by itself. Encoding UTF-8 is used.
   * This method closes the OutputStream at the end.
   */
  def serialize(saxonDoc: SaxonDocument, os: OutputStream): Unit = {
    val encoding = Codec.UTF8.toString

    // Rather weird serialization. No indent, but respecting the "ignorable" whitespace in the DOM tree.
    // Also, making sure there is a newline after the XML declaration.

    os.write(s"""<?xml version="1.0" encoding="$encoding"?>\n""".getBytes(encoding))
    os.flush()

    serialize(
      saxonDoc,
      os, { serializer =>
        serializer.setOutputProperty(Serializer.Property.METHOD, "xml")
        serializer.setOutputProperty(Serializer.Property.OMIT_XML_DECLARATION, "yes")
        serializer.setOutputProperty(Serializer.Property.ENCODING, encoding)
        serializer.setOutputProperty(Serializer.Property.INDENT, "no")
      }
    )
  }
}
