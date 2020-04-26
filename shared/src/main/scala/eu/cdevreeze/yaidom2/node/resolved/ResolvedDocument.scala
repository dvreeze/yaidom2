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

import eu.cdevreeze.yaidom2.creationapi.ClarkDocumentFactory
import eu.cdevreeze.yaidom2.queryapi.ClarkDocumentApi

/**
 * Document holding a ResolvedNodes.Elem.
 *
 * @author Chris de Vreeze
 */
final case class ResolvedDocument(docUriOption: Option[URI], documentElement: ResolvedNodes.Elem) extends ClarkDocumentApi {

  type NodeType = ResolvedNodes.Node

  type CanBeDocumentChildType = ResolvedNodes.CanBeDocumentChild

  type ElemType = ResolvedNodes.Elem

  type ThisDoc = ResolvedDocument

  def children: Seq[ResolvedNodes.CanBeDocumentChild] = Seq(documentElement)
}

object ResolvedDocument extends ClarkDocumentFactory {

  type TargetDocumentType = ResolvedDocument

  def from(document: ClarkDocumentApi): ResolvedDocument = {
    ResolvedDocument(document.docUriOption, ResolvedNodes.Elem.from(document.documentElement))
  }
}
