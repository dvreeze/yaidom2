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

import java.net.URI

import scala.collection.immutable.ArraySeq
import scala.jdk.OptionConverters._
import scala.jdk.StreamConverters._

import eu.cdevreeze.yaidom2.queryapi.oo.BackingDocumentApi
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.XdmNodeKind
import net.sf.saxon.s9api.streams.Predicates.isElement
import net.sf.saxon.s9api.streams.Steps.child

/**
 * Document holding a SaxonNodes.Elem.
 *
 * @author Chris de Vreeze
 */
final case class SaxonDocument(xdmNode: XdmNode) extends BackingDocumentApi {
  require(
    xdmNode.getNodeKind == XdmNodeKind.DOCUMENT,
    s"Expected document but got node of kind ${xdmNode.getNodeKind}")

  type NodeType = SaxonNodes.Node

  type CanBeDocumentChildType = SaxonNodes.CanBeDocumentChild

  type ElemType = SaxonNodes.Elem

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
}
