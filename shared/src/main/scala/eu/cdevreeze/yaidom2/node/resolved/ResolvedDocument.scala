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

package eu.cdevreeze.yaidom2.node.resolved

import java.net.URI

import eu.cdevreeze.yaidom2.queryapi.oo.BackingDocumentApi
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkDocumentApi
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.oo.Nodes

/**
 * Document holding a ResolvedNodes.Elem.
 *
 * @author Chris de Vreeze
 */
final case class ResolvedDocument(docUriOption: Option[URI], children: Seq[ResolvedNodes.CanBeDocumentChild]) extends ClarkDocumentApi {
  require(
    children.collect { case e: ResolvedNodes.Elem => e }.size == 1,
    s"A document must have precisely 1 document element but found ${children.collect { case e: ResolvedNodes.Elem => e }.size} ones")

  type NodeType = ResolvedNodes.Node

  type CanBeDocumentChildType = ResolvedNodes.CanBeDocumentChild

  type ElemType = ResolvedNodes.Elem

  def documentElement: ElemType = children.collectFirst { case e: ResolvedNodes.Elem => e }.get
}

object ResolvedDocument {

  def apply(uriOption: Option[URI], documentElement: ResolvedNodes.Elem): ResolvedDocument = {
    apply(uriOption, Seq(documentElement))
  }

  def from(uriOption: Option[URI], document: ClarkDocumentApi): ResolvedDocument = {
    val docChildren = document.children.collect { case ch: ClarkNodes.CanBeDocumentChild => ch }
    val targetDocChildren =
      docChildren.filter(_.nodeKind == Nodes.ElementKind).map(n => ResolvedNodes.CanBeDocumentChild.from(n))

    ResolvedDocument(uriOption, targetDocChildren)
  }

  def from(document: BackingDocumentApi): ResolvedDocument = {
    from(document.docUriOption, document)
  }
}
