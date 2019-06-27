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

package eu.cdevreeze.yaidom2.updateapi

import eu.cdevreeze.yaidom2.queryapi.ClarkNodes

/**
 * Transformable element API.
 *
 * @author Chris de Vreeze
 */
trait TransformableElemApi extends ClarkNodes.Elem {

  type ThisNode >: ThisElem <: ClarkNodes.Node

  type ThisElem <: TransformableElemApi

  /**
   * Returns the same element, except that child elements have been replaced by applying the given function. Non-element
   * child nodes occur in the result element unaltered.
   */
  def transformChildElems(f: ThisElem => ThisElem): ThisElem

  /**
   * Returns the same element, except that child elements have been replaced by applying the given function. Non-element
   * child nodes occur in the result element unaltered.
   */
  def transformChildElemsToNodeSeq(f: ThisElem => Seq[ThisNode]): ThisElem

  /**
   * Transforms the element by applying the given function to all its descendant-or-self elements, in a bottom-up manner.
   *
   * That is, returns the equivalent of:
   * {{{
   * f(transformChildElems(_.transformDescendantElemsOrSelf(f)))
   * }}}
   *
   * In other words, returns the equivalent of:
   * {{{
   * f(transformDescendantElems(f))
   * }}}
   */
  def transformDescendantElemsOrSelf(f: ThisElem => ThisElem): ThisElem

  /**
   * Transforms the element by applying the given function to all its descendant elements, in a bottom-up manner.
   *
   * That is, returns the equivalent of:
   * {{{
   * transformChildElems(_.transformDescendantElemsOrSelf(f))
   * }}}
   */
  def transformDescendantElems(f: ThisElem => ThisElem): ThisElem

  /**
   * Transforms each descendant element to a node sequence by applying the given function to all its descendant-or-self elements,
   * in a bottom-up manner.
   *
   * That is, returns the equivalent of:
   * {{{
   * f(transformChildElemsToNodeSeq(_.transformDescendantElemsOrSelfToNodeSeq(f)))
   * }}}
   *
   * In other words, returns the equivalent of:
   * {{{
   * f(transformDescendantElemsToNodeSeq(f))
   * }}}
   */
  def transformDescendantElemsOrSelfToNodeSeq(f: ThisElem => Seq[ThisNode]): Seq[ThisNode]

  /**
   * Transforms each descendant element to a node sequence by applying the given function to all its descendant elements,
   * in a bottom-up manner. The function is not applied to this element itself.
   *
   * That is, returns the equivalent of:
   * {{{
   * transformChildElemsToNodeSeq(_.transformDescendantElemsOrSelfToNodeSeq(f))
   * }}}
   *
   * It is equivalent to the following expression:
   * {{{
   * transformDescendantElemsOrSelf(_.transformChildElemsToNodeSeq(f))
   * }}}
   */
  def transformDescendantElemsToNodeSeq(f: ThisElem => Seq[ThisNode]): ThisElem
}

object TransformableElemApi {

  /**
   * This update API type, restricting Node and Elem to the passed type parameters.
   *
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[N, E] = TransformableElemApi { type ThisNode = N; type ThisElem = E }
}
