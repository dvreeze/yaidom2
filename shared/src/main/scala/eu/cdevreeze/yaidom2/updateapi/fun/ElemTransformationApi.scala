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

package eu.cdevreeze.yaidom2.updateapi.fun

/**
 * Element transformation API.
 *
 * @author Chris de Vreeze
 */
trait ElemTransformationApi {

  type NodeType

  type ElemType <: NodeType

  /**
   * Returns the same element, except that child elements have been replaced by applying the given function. Non-element
   * child nodes occur in the result element unaltered.
   */
  def transformChildElems(elem: ElemType, f: ElemType => ElemType): ElemType

  /**
   * Returns the same element, except that child elements have been replaced by applying the given function. Non-element
   * child nodes occur in the result element unaltered.
   */
  def transformChildElemsToNodeSeq(elem: ElemType, f: ElemType => Seq[NodeType]): ElemType

  /**
   * Transforms the element by applying the given function to all its descendant-or-self elements, in a bottom-up manner.
   *
   * That is, returns the equivalent of:
   * {{{
   * f(transformChildElems(elem, e => transformDescendantElemsOrSelf(e, f)))
   * }}}
   *
   * In other words, returns the equivalent of:
   * {{{
   * f(transformDescendantElems(elem, f))
   * }}}
   */
  def transformDescendantElemsOrSelf(elem: ElemType, f: ElemType => ElemType): ElemType

  /**
   * Transforms the element by applying the given function to all its descendant elements, in a bottom-up manner.
   *
   * That is, returns the equivalent of:
   * {{{
   * transformChildElems(elem, e => transformDescendantElemsOrSelf(e, f))
   * }}}
   */
  def transformDescendantElems(elem: ElemType, f: ElemType => ElemType): ElemType

  /**
   * Transforms each descendant element to a node sequence by applying the given function to all its descendant-or-self elements,
   * in a bottom-up manner.
   *
   * That is, returns the equivalent of:
   * {{{
   * f(transformChildElemsToNodeSeq(elem, e => transformDescendantElemsOrSelfToNodeSeq(e, f)))
   * }}}
   *
   * In other words, returns the equivalent of:
   * {{{
   * f(transformDescendantElemsToNodeSeq(elem, f))
   * }}}
   */
  def transformDescendantElemsOrSelfToNodeSeq(elem: ElemType, f: ElemType => Seq[NodeType]): Seq[NodeType]

  /**
   * Transforms each descendant element to a node sequence by applying the given function to all its descendant elements,
   * in a bottom-up manner. The function is not applied to this element itself.
   *
   * That is, returns the equivalent of:
   * {{{
   * transformChildElemsToNodeSeq(elem, e => transformDescendantElemsOrSelfToNodeSeq(e, f))
   * }}}
   *
   * It is equivalent to the following expression:
   * {{{
   * transformDescendantElemsOrSelf(elem, { e => transformChildElemsToNodeSeq(e, f) })
   * }}}
   */
  def transformDescendantElemsToNodeSeq(elem: ElemType, f: ElemType => Seq[NodeType]): ElemType
}

object ElemTransformationApi {

  /**
   * This query API type, restricting NodeType and ElemType to the passed type parameters.
   *
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[N, E] = ElemTransformationApi { type NodeType = N; type ElemType = E }
}
