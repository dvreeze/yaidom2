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

package eu.cdevreeze.yaidom2.updateapi.oo

/**
 * Updatable element API. It is to a large extent centered around "navigation paths". For an explanation of these navigation
 * paths, see for example method `ElemApi.findDescendantElemOrSelf`.
 *
 * @author Chris de Vreeze
 */
trait UpdatableElemApi {

  type ThisNode

  type ThisElem <: ThisNode with UpdatableElemApi

  // Functional update methods taking 1 navigation path or step.

  /**
   * Functionally updates this element tree at the given (child element) navigation step (relative to this element).
   */
  def updateChildElem(navigationStep: Int)(f: ThisElem => ThisElem): ThisElem

  /**
   * Returns `updateChildElem(navigationStep)(_ => newElem)`.
   */
  def updateChildElem(navigationStep: Int, newElem: ThisElem): ThisElem

  /**
   * Functionally updates this element tree at the given (child element) navigation step (relative to this element).
   */
  def updateChildElemWithNodeSeq(navigationStep: Int)(f: ThisElem => Seq[ThisNode]): ThisElem

  /**
   * Returns `updateChildElemWithNodeSeq(navigationStep)(_ => newNodes)`.
   */
  def updateChildElemWithNodeSeq(navigationStep: Int, newNodes: Seq[ThisNode]): ThisElem

  /**
   * Functionally updates this element tree at the given (descendant-or-self element) navigation path (relative to this element).
   */
  def updateDescendantElemOrSelf(navigationPath: Seq[Int])(f: ThisElem => ThisElem): ThisElem

  /**
   * Returns `updateDescendantElemOrSelf(navigationPath)(_ => newElem)`.
   */
  def updateDescendantElemOrSelf(navigationPath: Seq[Int], newElem: ThisElem): ThisElem

  /**
   * Functionally updates this element tree at the given (descendant element) navigation path (relative to this element).
   * Only descendant elements are updated. If the navigation path is empty, pointing to this element itself, no update
   * takes place and this element itself is returned.
   */
  def updateDescendantElemWithNodeSeq(navigationPath: Seq[Int])(f: ThisElem => Seq[ThisNode]): ThisElem

  /**
   * Returns `updateDescendantElemWithNodeSeq(navigationPath)(_ => newNodes)`.
   */
  def updateDescendantElemWithNodeSeq(navigationPath: Seq[Int], newNodes: Seq[ThisNode]): ThisElem

  // Functional update methods taking sets of navigation paths or steps.

  /**
   * Functionally updates this element tree at the given navigation steps (relative to this element). It is equivalent
   * to calling method `updateChildElem(navigationStep)(f)` repeatedly, but in reverse document order (in order not to
   * undo earlier updates).
   *
   * For efficiency it is best to pass only small sets of navigation steps.
   */
  def updateChildElems(navigationSteps: Set[Int])(f: (ThisElem, Int) => ThisElem): ThisElem

  /**
   * Functionally updates this element tree at the given navigation steps (relative to this element). It is equivalent
   * to calling method `updateChildElem(navigationStep)(f)` repeatedly, but in reverse document order (in order not to
   * undo earlier updates).
   *
   * For efficiency it is best to pass only small sets of navigation steps.
   */
  def updateChildElemsWithNodeSeq(navigationSteps: Set[Int])(f: (ThisElem, Int) => Seq[ThisNode]): ThisElem

  /**
   * Functionally updates this element tree at the given navigation paths (relative to this element). It is equivalent
   * to calling method `updateDescendantElemOrSelf(navigationPath)(f)` repeatedly, but in reverse document order (in order not to
   * undo earlier updates).
   *
   * For efficiency it is best to pass only small sets of navigation paths.
   */
  def updateDescendantElemsOrSelf(navigationPaths: Set[Seq[Int]])(f: (ThisElem, Seq[Int]) => ThisElem): ThisElem

  /**
   * Functionally updates this element tree at the given navigation paths (relative to this element). It is equivalent
   * to calling method `updateDescendantElemWithNodeSeq(navigationPath)(f)` repeatedly, but in reverse document order (in order not to
   * undo earlier updates). Hence, the empty path is ignored (since only descendant elements are updated).
   *
   * For efficiency it is best to pass only small sets of navigation paths. Only descendant elements are updated, so
   * the empty path, if passed, is ignored.
   */
  def updateDescendantElemsWithNodeSeq(navigationPaths: Set[Seq[Int]])(f: (ThisElem, Seq[Int]) => Seq[ThisNode]): ThisElem

  // Functional update methods taking partial update functions.

  /**
   * Calls `updateChildElems(navigationSteps)(f)`, where the set of navigation steps is the set of steps for which
   * the partial update function is defined.
   */
  def updateChildElems(f: PartialFunction[(ThisElem, Int), ThisElem]): ThisElem

  /**
   * Calls `updateChildElemsWithNodeSeq(navigationSteps)(f)`, where the set of navigation steps is the set of steps for which
   * the partial update function is defined.
   */
  def updateChildElemsWithNodeSeq(f: PartialFunction[(ThisElem, Int), Seq[ThisNode]]): ThisElem

  /**
   * Calls `updateDescendantElemsOrSelf(navigationPaths)(f)`, where the set of navigation paths is the set of topmost element
   * navigation paths for which the partial update function is defined.
   */
  def updateTopmostElemsOrSelf(f: PartialFunction[(ThisElem, Seq[Int]), ThisElem]): ThisElem

  /**
   * Calls `updateDescendantElemsWithNodeSeq(navigationPaths)(f)`, where the set of navigation paths is the set of topmost non-empty element
   * navigation paths for which the partial update function is defined. Hence, the empty path is ignored.
   */
  def updateTopmostElemsWithNodeSeq(f: PartialFunction[(ThisElem, Seq[Int]), Seq[ThisNode]]): ThisElem
}

object UpdatableElemApi {

  /**
   * This update API type, restricting Node and Elem to the passed type parameters.
   *
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[N, E] = UpdatableElemApi { type ThisNode = N; type ThisElem = E }
}
