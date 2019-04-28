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

package eu.cdevreeze.yaidom2.queryapi.oo

/**
 * Common contract for documents containing ClarkNodes elements.
 *
 * @author Chris de Vreeze
 */
trait ClarkDocumentApi extends DocumentApi {

  type NodeType <: ClarkNodes.Node

  type CanBeDocumentChildType <: NodeType with ClarkNodes.CanBeDocumentChild

  type ElemType <: CanBeDocumentChildType with ClarkNodes.Elem
}

object ClarkDocumentApi {

  type Aux[N, C, E] = ClarkDocumentApi {
    type Nodetype = N
    type CanBeDocumentChildType = C
    type ElemType = E
  }
}
