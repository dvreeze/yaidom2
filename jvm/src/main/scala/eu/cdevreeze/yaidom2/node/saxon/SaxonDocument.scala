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
import java.io.OutputStream
import java.io.Writer
import java.net.URI

import eu.cdevreeze.yaidom2.queryapi.BackingDocumentApi
import net.sf.saxon.s9api.Serializer
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.XdmNodeKind
import net.sf.saxon.s9api.streams.Predicates.isElement
import net.sf.saxon.s9api.streams.Steps.child

import scala.collection.immutable.ArraySeq
import scala.jdk.OptionConverters._
import scala.jdk.StreamConverters._

/**
 * Document holding a SaxonNodes.Elem.
 *
 * @author Chris de Vreeze
 */
final case class SaxonDocument(xdmNode: XdmNode) extends BackingDocumentApi {
  require(xdmNode.getNodeKind == XdmNodeKind.DOCUMENT, s"Expected document but got node of kind ${xdmNode.getNodeKind}")

  type NodeType = SaxonNodes.Node

  type CanBeDocumentChildType = SaxonNodes.CanBeDocumentChild

  type ElemType = SaxonNodes.Elem

  type ThisDoc = SaxonDocument

  def docUriOption: Option[URI] = {
    Option(xdmNode.getDocumentURI)
  }

  def children: Seq[SaxonNodes.CanBeDocumentChild] = {
    val stream = xdmNode.select(child())
    stream.toScala(ArraySeq).flatMap(n => SaxonNodes.CanBeDocumentChild.opt(n))
  }

  def documentElement: ElemType = {
    val stream = xdmNode.select(child().where(n => isElement.test(n)))
    stream.findFirst.toScala.map(n => SaxonNodes.Elem(n)).get
  }

  // JVM-only, but so is Saxon itself

  def newSerializer(stream: OutputStream): Serializer = {
    xdmNode.getProcessor.newSerializer(stream)
  }

  def newSerializer(writer: Writer): Serializer = {
    xdmNode.getProcessor.newSerializer(writer)
  }

  def newSerializer(file: File): Serializer = {
    xdmNode.getProcessor.newSerializer(file)
  }
}
