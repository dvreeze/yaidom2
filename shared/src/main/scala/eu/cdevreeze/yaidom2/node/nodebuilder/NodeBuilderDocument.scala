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

package eu.cdevreeze.yaidom2.node.nodebuilder

import java.net.URI

import eu.cdevreeze.yaidom2.creationapi.ScopedDocumentFactory
import eu.cdevreeze.yaidom2.queryapi.Nodes
import eu.cdevreeze.yaidom2.queryapi.ScopedDocumentApi
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes

/**
 * Document holding a NodeBuilders.Elem.
 *
 * @author Chris de Vreeze
 */
final case class NodeBuilderDocument(docUriOption: Option[URI], children: Seq[CanBeDocumentChild]) extends ScopedDocumentApi {
  require(
    children.collect { case e: Elem => e }.size == 1,
    s"A document must have precisely 1 document element but found ${children.collect { case e: Elem => e }.size} ones")

  type NodeType = Node

  type CanBeDocumentChildType = CanBeDocumentChild

  type ElemType = Elem

  def documentElement: ElemType = children.collectFirst { case e: Elem => e }.get
}

object NodeBuilderDocument extends ScopedDocumentFactory {

  type TargetDocumentType = NodeBuilderDocument

  def apply(docUriOption: Option[URI], documentElement: Elem): NodeBuilderDocument = {
    apply(docUriOption, Seq(documentElement))
  }

  /**
   * Returns a copy of the passed document as "creation DSL" document.
   * If the document element of the passed document does not meet the requirements of "creation DSL" elements, an exception is thrown.
   * Such requirements include the absence of any default namespace, the absence of namespace undeclarations, etc.
   */
  def from(document: ScopedDocumentApi): NodeBuilderDocument = {
    val docChildren = document.children.collect { case ch: ScopedNodes.CanBeDocumentChild => ch }

    val targetDocChildren =
      docChildren
        .filter(n => Set[Nodes.NodeKind](Nodes.ElementKind).contains(n.nodeKind))
        .map(n => Elem.from(n.asInstanceOf[ScopedNodes.Elem])) // TODO Replace this expensive conversion

    NodeBuilderDocument(document.docUriOption, targetDocChildren)
  }
}
