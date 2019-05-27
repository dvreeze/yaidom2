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

import eu.cdevreeze.yaidom2.queryapi.ElemStep

/**
 * '''Core API''' for element nodes that offer the central `ClarkElemApi` query API. Each element implementation should
 * directly or indirectly implement this API.
 *
 * This API is directly implemented by elements that know about expanded names but not about qualified names.
 *
 * @author Chris de Vreeze
 */
object ClarkNodes {

  /**
   * Arbitrary node
   */
  trait Node extends Nodes.Node

  /**
   * Potential document child, so an element, processing instruction or comment
   */
  trait CanBeDocumentChild extends Node with Nodes.CanBeDocumentChild

  /**
   * Arbitrary element node, offering the `ClarkElemApi` element query API (and more).
   *
   * Method `select` makes it possible to use the `ElemStep` API on "Clark element nodes".
   */
  trait Elem extends CanBeDocumentChild with Nodes.Elem with ClarkElemApi {

    type ThisElem <: Elem

    type ThisNode >: ThisElem <: Node

    /**
     * Returns all child nodes, of any kind of node (element node, text node etc.).
     */
    def children: Seq[ThisNode]

    /**
     * Applies the given element step to this element.
     */
    def select(step: ElemStep[ThisElem]): Seq[ThisElem]
  }

  /**
   * Arbitrary text node
   */
  trait Text extends Node with Nodes.Text

  /**
   * Arbitrary comment node
   */
  trait Comment extends CanBeDocumentChild with Nodes.Comment

  /**
   * Arbitrary processing instruction node
   */
  trait ProcessingInstruction extends CanBeDocumentChild with Nodes.ProcessingInstruction

  object Elem {

    /**
     * This query API type, restricting Node and Elem to the passed type parameters.
     *
     * @tparam N The node type
     * @tparam E The element type
     */
    type Aux[N, E] = Elem { type ThisNode = N; type ThisElem = E }
  }
}
