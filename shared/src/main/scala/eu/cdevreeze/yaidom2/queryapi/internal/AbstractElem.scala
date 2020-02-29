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

import scala.collection.mutable

import eu.cdevreeze.yaidom2.queryapi.ElemApi

/**
 * Abstract partially implemented ElemApi, for re-usable (but overridable) partial element implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not as stable as the purely abstract API.
 *
 * In concrete element classes extending this trait (directly or indirectly), strongly consider overriding all methods
 * that contain type member ThisElem anywhere in the method signature, by just calling the super-trait version of the method.
 * That would ensure that in those method signatures type member ThisElem has the correct concrete element type.
 *
 * @author Chris de Vreeze
 */
trait AbstractElem extends ElemApi {

  type ThisElem <: AbstractElem.Aux[ThisElem]

  protected[yaidom2] def self: ThisElem

  protected[yaidom2] def toImmutableSeq(xs: collection.Seq[ThisElem]): Seq[ThisElem]

  // ElemApi

  def findAllChildElems: Seq[ThisElem] = {
    filterChildElems(_ => true)
  }

  def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    findAllChildElems.flatMap(_.filterDescendantElemsOrSelf(p))
  }

  def findAllDescendantElems: Seq[ThisElem] = {
    filterDescendantElems(_ => true)
  }

  def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
    findAllChildElems.view.flatMap(_.findDescendantElemOrSelf(p)).headOption
  }

  def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
    val result = mutable.ArrayBuffer[ThisElem]()

    def accumulate(elm: ThisElem): Unit = {
      if (p(elm)) result += elm
      // Recursive calls (not tail-recursive, but the depth is typically limited)
      elm.findAllChildElems.foreach(accumulate)
    }

    accumulate(self)
    toImmutableSeq(result)
  }

  def findAllDescendantElemsOrSelf: Seq[ThisElem] = {
    filterDescendantElemsOrSelf(_ => true)
  }

  def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
    var result: Option[ThisElem] = None

    def findElem(elm: ThisElem): Unit = {
      if (result.isEmpty) {
        if (p(elm)) result = Some(elm)
      }
      if (result.isEmpty) {
        // Recursive calls (not tail-recursive, but the depth is typically limited)
        elm.findAllChildElems.foreach(findElem)
      }
    }

    findElem(self)
    result
  }

  def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    findAllChildElems.flatMap(_.findTopmostElemsOrSelf(p))
  }

  def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
    val result = mutable.ArrayBuffer[ThisElem]()

    def accumulate(elm: ThisElem): Unit = {
      if (p(elm)) {
        result += elm
      } else {
        // Recursive calls (not tail-recursive, but the depth is typically limited)
        elm.findAllChildElems.foreach(accumulate)
      }
    }

    accumulate(self)
    toImmutableSeq(result)
  }

  def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = {
    findDescendantElemOrSelf(navigationPath).getOrElse(sys.error(s"Missing element at navigation path $navigationPath"))
  }
}

object AbstractElem {

  type Aux[E] = AbstractElem {type ThisElem = E}
}
