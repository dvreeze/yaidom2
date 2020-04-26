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

package eu.cdevreeze.yaidom2.queryapi

import java.net.URI

/**
 * Common contract for documents.
 *
 * @author Chris de Vreeze
 */
trait DocumentApi {

  type NodeType <: Nodes.Node

  type CanBeDocumentChildType <: NodeType with Nodes.CanBeDocumentChild

  type ElemType <: CanBeDocumentChildType with Nodes.Elem

  type ThisDoc <: DocumentApi

  def docUriOption: Option[URI]

  def children: Seq[CanBeDocumentChildType]

  def documentElement: ElemType
}

object DocumentApi {

  type Aux[N, C, E, D] = DocumentApi {
    type NodeType = N
    type CanBeDocumentChildType = C
    type ElemType = E
    type ThisDoc = D
  }
}
