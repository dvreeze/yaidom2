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

package eu.cdevreeze.yaidom2.queryapi.fun

/**
 * Element function API. See `ElemApi`, but this is its non-OO functional counterpart.
 *
 * @author Chris de Vreeze
 */
trait ElemFunctionsApi {

  type ElemType

  def filterChildElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAllChildElems(elem: ElemType): Seq[ElemType]

  def findChildElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterDescendantElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAllDescendantElems(elem: ElemType): Seq[ElemType]

  def findDescendantElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterDescendantElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAllDescendantElemsOrSelf(elem: ElemType): Seq[ElemType]

  def findDescendantElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def findTopmostElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findTopmostElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  /**
   * Returns the optional descendant-or-self element at the given navigation path. If the navigation path is Seq(3, 5, 0),
   * the first navigation step is to the child element at (element) index 3, zero-based, the next navigation step is
   * to its child element at zero-based (element) index 5, and the last navigation step is to the latter's child element
   * at zero-based (element) index 0.
   *
   * If the navigation path is out of bounds in one of the steps, None is returned.
   */
  def findDescendantElemOrSelf(elem: ElemType, navigationPath: Seq[Int]): Option[ElemType]

  /**
   * Returns the descendant-or-self element at the given navigation path. If the navigation path is Seq(3, 5, 0),
   * the first navigation step is to the child element at (element) index 3, zero-based, the next navigation step is
   * to its child element at zero-based (element) index 5, and the last navigation step is to the latter's child element
   * at zero-based (element) index 0.
   *
   * If the navigation path is out of bounds in one of the steps, an exception is thrown.
   */
  def getDescendantElemOrSelf(elem: ElemType, navigationPath: Seq[Int]): ElemType
}

object ElemFunctionsApi {

  type Aux[E] = ElemFunctionsApi {
    type ElemType = E
  }
}
