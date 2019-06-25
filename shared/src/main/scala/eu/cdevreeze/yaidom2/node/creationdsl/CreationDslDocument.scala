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

package eu.cdevreeze.yaidom2.node.creationdsl

import java.net.URI

import eu.cdevreeze.yaidom2.creationapi.ScopedDocumentFactory
import eu.cdevreeze.yaidom2.queryapi.Nodes
import eu.cdevreeze.yaidom2.queryapi.ScopedDocumentApi
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes

/**
 * Document holding a CreationDslNodes.Elem.
 *
 * @author Chris de Vreeze
 */
final case class CreationDslDocument(docUriOption: Option[URI], children: Seq[CreationDslNodes.CanBeDocumentChild]) extends ScopedDocumentApi {
  require(
    children.collect { case e: CreationDslNodes.Elem => e }.size == 1,
    s"A document must have precisely 1 document element but found ${children.collect { case e: CreationDslNodes.Elem => e }.size} ones")

  type NodeType = CreationDslNodes.Node

  type CanBeDocumentChildType = CreationDslNodes.CanBeDocumentChild

  type ElemType = CreationDslNodes.Elem

  def documentElement: ElemType = children.collectFirst { case e: CreationDslNodes.Elem => e }.get
}

object CreationDslDocument extends ScopedDocumentFactory {

  type TargetDocumentType = CreationDslDocument

  def apply(docUriOption: Option[URI], documentElement: CreationDslNodes.Elem): CreationDslDocument = {
    apply(docUriOption, Seq(documentElement))
  }

  def from(document: ScopedDocumentApi): CreationDslDocument = {
    val docChildren = document.children.collect { case ch: ScopedNodes.CanBeDocumentChild => ch }

    val targetDocChildren =
      docChildren.filter(n => Set[Nodes.NodeKind](Nodes.ElementKind).contains(n.nodeKind))
        .map(n => CreationDslNodes.Elem.from(n.asInstanceOf[ScopedNodes.Elem]))

    CreationDslDocument(document.docUriOption, targetDocChildren)
  }
}
