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

package eu.cdevreeze.yaidom2.queryapi.propertytests

import eu.cdevreeze.yaidom2.queryapi.oo.ElemApi
import org.scalacheck.Arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

abstract class ElemApiSpecification[E <: ElemApi.Aux[E]] extends Properties("ElemApi") {

  implicit def arbitraryElem: Arbitrary[E]

  implicit def arbitraryPred: Arbitrary[E => Boolean]

  property("filterChildElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterChildElems(pred) == elem.filterChildElems(_ => true).filter(pred)
  }

  property("filterDescendantElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterDescendantElems(pred) == elem.filterChildElems(_ => true).flatMap(_.filterDescendantElemsOrSelf(pred))
  }

  property("filterDescendantElemsOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    // Recursive calls
    elem.filterDescendantElemsOrSelf(pred) ==
      Seq(elem).filter(pred) ++ elem.filterChildElems(_ => true).flatMap(_.filterDescendantElemsOrSelf(pred))
  }

  property("findChildElem") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findChildElem(pred) == elem.filterChildElems(pred).headOption
  }

  property("findDescendantElem") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findDescendantElem(pred) == elem.filterDescendantElems(pred).headOption
  }

  property("findDescendantElemOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findDescendantElemOrSelf(pred) == elem.filterDescendantElemsOrSelf(pred).headOption
  }
}
