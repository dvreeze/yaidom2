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

package eu.cdevreeze.yaidom2.node.jsdom

import java.net.URI

import eu.cdevreeze.yaidom2.queryapi.oo.BackingDocumentApi
import org.scalajs.dom

/**
 * Document holding a JsDomNodes.Elem.
 *
 * @author Chris de Vreeze
 */
final case class JsDomDocument(jsDomDocument: dom.Document) extends BackingDocumentApi {

  type NodeType = JsDomNodes.Node

  type CanBeDocumentChildType = JsDomNodes.CanBeDocumentChild

  type ElemType = JsDomNodes.Elem

  def docUriOption: Option[URI] = {
    Option(jsDomDocument.documentURI).map(s => new URI(s))
  }

  def children: Seq[JsDomNodes.CanBeDocumentChild] = {
    val childrenNodeList = jsDomDocument.childNodes

    JsDomNodes.Node.nodeListToSeq(childrenNodeList).flatMap { node =>
      JsDomNodes.CanBeDocumentChild.opt(node)
    }
  }

  def documentElement: ElemType = {
    JsDomNodes.Elem(jsDomDocument.documentElement)
  }
}
