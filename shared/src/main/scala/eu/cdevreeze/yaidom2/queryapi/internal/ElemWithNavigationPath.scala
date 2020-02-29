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

package eu.cdevreeze.yaidom2.queryapi.internal

import scala.collection.immutable.ArraySeq

import eu.cdevreeze.yaidom2.queryapi.ElemApi

/**
 * Element with its navigation path relative to some (unseen) root. It is used internally to implement some of the methods
 * in the functional update API.
 *
 * @author Chris de Vreeze
 */
final class ElemWithNavigationPath[E <: ElemApi.Aux[E]](val elem: E, val navigationPath: Seq[Int]) extends AbstractElem {

  type ThisElem = ElemWithNavigationPath[E]

  protected[yaidom2] def self: ThisElem = ElemWithNavigationPath.this

  protected[yaidom2] def toImmutableSeq(xs: collection.Seq[ThisElem]): Seq[ThisElem] = xs.to(ArraySeq)

  def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    elem.findAllChildElems.zipWithIndex.map { case (che, idx) =>
      new ElemWithNavigationPath[E](che, navigationPath.appended(idx))
    }.filter(p)
  }

  def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
    filterChildElems(p).headOption
  }

  def findDescendantElemOrSelf(navigationPath: Seq[Int]): Option[ThisElem] = {
    if (navigationPath.isEmpty) {
      Some(self)
    } else {
      val childElemIdx: Int = navigationPath(0)
      val childElems: Seq[ThisElem] = findAllChildElems

      if (childElemIdx >= 0 && childElemIdx < childElems.size) {
        // Recursive call
        Option(childElems(childElemIdx)).flatMap(_.findDescendantElemOrSelf(navigationPath.drop(1)))
      } else {
        None
      }
    }
  }

  // Overriding methods that have type member ThisElem in the method signature, to "correct" the method signature now that ThisElem is known

  override def findAllChildElems: Seq[ThisElem] = super.findAllChildElems

  override def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = super.filterDescendantElems(p)

  override def findAllDescendantElems: Seq[ThisElem] = super.findAllDescendantElems

  override def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = super.findDescendantElem(p)

  override def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.filterDescendantElemsOrSelf(p)

  override def findAllDescendantElemsOrSelf: Seq[ThisElem] = super.findAllDescendantElemsOrSelf

  override def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = super.findDescendantElemOrSelf(p)

  override def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = super.findTopmostElems(p)

  override def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.findTopmostElemsOrSelf(p)

  override def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = super.getDescendantElemOrSelf(navigationPath)
}

object ElemWithNavigationPath {

  def apply[E <: ElemApi.Aux[E]](elem: E): ElemWithNavigationPath[E] = {
    new ElemWithNavigationPath[E](elem, Seq.empty)
  }
}
