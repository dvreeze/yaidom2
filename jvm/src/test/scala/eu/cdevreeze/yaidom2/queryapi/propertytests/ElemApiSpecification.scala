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
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

trait ElemApiSpecification[E <: ElemApi.Aux[E]] extends ElemApiSpecificationDataProvider[E] {
  self: Properties =>

  // "Definitions" of ElemApi methods

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

  property("findTopmostElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findTopmostElems(pred) == elem.filterChildElems(_ => true).flatMap(_.findTopmostElemsOrSelf(pred))
  }

  property("findTopmostElemsOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findTopmostElemsOrSelf(pred) == findTopmostElemsOrSelf(elem, pred)
  }

  // Other properties

  property("filterDescendantElems-in-terms-of-filter") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterDescendantElems(pred) == elem.filterDescendantElems(_ => true).filter(pred)
  }

  property("filterDescendantElemsOrSelf-in-terms-of-filter") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterDescendantElemsOrSelf(pred) == elem.filterDescendantElemsOrSelf(_ => true).filter(pred)
  }

  property("filterDescendantElems-in-terms-of-findTopmostElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterDescendantElems(pred) ==
      elem.findTopmostElems(pred).flatMap(_.filterDescendantElemsOrSelf(pred))
  }

  property("filterDescendantElemsOrSelf-in-terms-of-findTopmostElemsOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterDescendantElemsOrSelf(pred) ==
      elem.findTopmostElemsOrSelf(pred).flatMap(_.filterDescendantElemsOrSelf(pred))
  }

  property("filterDescendantElemsOrSelf-in-terms-of-filterDescendantElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterDescendantElemsOrSelf(pred) ==
      Seq(elem).filter(pred) ++ elem.filterDescendantElems(pred)
  }

  property("findTopmostElemsOrSelf-in-terms-of-findTopmostElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findTopmostElemsOrSelf(pred) == {
      if (pred(elem)) Seq(elem) else elem.findTopmostElems(pred)
    }
  }

  // Leaning on value equality for elements

  property("filterDescendantElems-in-terms-of-filterDescendantElemsOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterDescendantElems(pred) ==
      elem.filterDescendantElemsOrSelf(pred).filterNot(Set(elem))
  }

  property("findDescendantElem-in-terms-of-findTopmostElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findDescendantElem(pred) == elem.findTopmostElems(pred).headOption
  }

  property("findDescendantElemOrSelf-in-terms-of-findTopmostElemsOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findDescendantElemOrSelf(pred) == elem.findTopmostElemsOrSelf(pred).headOption
  }

  private def findTopmostElemsOrSelf(elm: E, p: E => Boolean): Seq[E] = {
    if (p(elm)) {
      Seq(elm)
    } else {
      // Recursive calls
      elm.filterChildElems(_ => true).flatMap(e => findTopmostElemsOrSelf(e, p))
    }
  }
}
