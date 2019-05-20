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
 * Element API. It knows how to find elements by following one of the axes descendant-or-self, descendant
 * or child. It does not know anything about XML, so this API could even be used for non-XML tree-like objects.
 *
 * Concrete element implementations that (in)directly implement this interface should have efficient implementations
 * of the methods offered by this API. These methods can in turn be used to implement element step factories.
 *
 * @author Chris de Vreeze
 */
trait ElemApi {

  type ThisElem <: ElemApi

  def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem]

  def findAllChildElems(): Seq[ThisElem]

  def findChildElem(p: ThisElem => Boolean): Option[ThisElem]

  def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem]

  def findAllDescendantElems(): Seq[ThisElem]

  def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem]

  def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem]

  def findAllDescendantElemsOrSelf(): Seq[ThisElem]

  def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem]

  def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem]

  def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem]

  /**
   * Returns the optional descendant-or-self element at the given navigation path. If the navigation path is Seq(3, 5, 0),
   * the first navigation step is to the child element at (element) index 3, zero-based, the next navigation step is
   * to its child element at zero-based (element) index 5, and the last navigation step is to the latter's child element
   * at zero-based (element) index 0.
   *
   * If the navigation path is out of bounds in one of the steps, None is returned.
   */
  def findDescendantElemOrSelf(navigationPath: Seq[Int]): Option[ThisElem]

  /**
   * Returns the descendant-or-self element at the given navigation path. If the navigation path is Seq(3, 5, 0),
   * the first navigation step is to the child element at (element) index 3, zero-based, the next navigation step is
   * to its child element at zero-based (element) index 5, and the last navigation step is to the latter's child element
   * at zero-based (element) index 0.
   *
   * If the navigation path is out of bounds in one of the steps, an exception is thrown.
   */
  def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem
}

object ElemApi {

  type Aux[E] = ElemApi {
    type ThisElem = E
  }
}
