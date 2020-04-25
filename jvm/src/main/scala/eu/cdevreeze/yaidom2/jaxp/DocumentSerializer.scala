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

package eu.cdevreeze.yaidom2.jaxp

import eu.cdevreeze.yaidom2.queryapi.ScopedDocumentApi
import javax.xml.transform.OutputKeys
import javax.xml.transform.Result
import javax.xml.transform.Transformer
import javax.xml.transform.TransformerFactory
import javax.xml.transform.sax.SAXTransformerFactory

import scala.io.Codec
import scala.util.Try

/**
 * Document serializer, leveraging SaxEventProducers. There are no methods to serialize document elements, but they are
 * not needed, since in most cases it is easy to create documents from document elements.
 *
 * @author Chris de Vreeze
 */
final class DocumentSerializer(val transformerFactory: SAXTransformerFactory) {

  /**
   * Serializes the given document. Note that the encoding should match the encoding used in the passed Result, if applicable.
   */
  def serializeDocument(doc: ScopedDocumentApi, result: Result, encoding: String): Unit = {
    def configure(tf: Transformer): Unit = { tf.setOutputProperty(OutputKeys.ENCODING, encoding) }

    serializeDocument(doc, result, configure(_))
  }

  /**
   * Serializes the given document. Be careful to match the Result and Transformer configuration on encoding, if applicable.
   */
  def serializeDocument(doc: ScopedDocumentApi, result: Result, configureTransformer: Transformer => Unit): Unit = {
    val th = transformerFactory.newTransformerHandler()
    configureTransformer(th.getTransformer)
    doc.docUriOption.foreach(docUri => th.setSystemId(docUri.toString))
    th.setResult(result)

    SaxEventProducers.produceEventsForDocument(doc, th)
  }

  /**
   * Configures the transformer for formatting (indent 2, if picked up), setting the encoding to UTF-8.
   */
  def configureForFormatting(transformer: Transformer): Unit = {
    transformer.setOutputProperty(OutputKeys.ENCODING, Codec.UTF8.toString)
    transformer.setOutputProperty(OutputKeys.INDENT, "yes")

    Try(transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2")).getOrElse(())
  }

  /**
   * Configures the transformer for omitting the XML declaration and no formatting, setting the encoding to UTF-8.
   */
  def configureForOmittingXmlDeclaration(transformer: Transformer): Unit = {
    transformer.setOutputProperty(OutputKeys.ENCODING, Codec.UTF8.toString)
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
  }
}

object DocumentSerializer {

  def apply(transformerFactory: TransformerFactory): DocumentSerializer = {
    new DocumentSerializer(transformerFactory.ensuring(_.isInstanceOf[SAXTransformerFactory]).asInstanceOf[SAXTransformerFactory])
  }
}
