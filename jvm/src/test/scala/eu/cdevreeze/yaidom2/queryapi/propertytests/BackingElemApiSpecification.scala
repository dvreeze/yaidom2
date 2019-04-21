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

import java.net.URI

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.queryapi.oo.steps.ElemSteps._
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Properties

trait BackingElemApiSpecification[N, E <: BackingNodes.Elem.Aux[N, E]] extends ScopedElemApiSpecification[N, E] {
  self: Properties =>

  // "Definitions" of BackingElemApi methods

  property("findParentElem") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findParentElem(pred) == elem.findParentElem(_ => true).filter(pred)
  }

  property("filterAncestorElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterAncestorElems(pred) ==
      elem.findParentElem(_ => true).toSeq.flatMap(_.filterAncestorElemsOrSelf(pred))
  }

  property("filterAncestorElemsOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    // Recursive calls
    elem.filterAncestorElemsOrSelf(pred) ==
      Seq(elem).filter(pred) ++ elem.findParentElem(_ => true).toSeq.flatMap(_.filterAncestorElemsOrSelf(pred))
  }

  property("findAncestorElem") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findAncestorElem(pred) == elem.filterAncestorElems(pred).headOption
  }

  property("findAncestorElemOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findAncestorElemOrSelf(pred) == elem.filterAncestorElemsOrSelf(pred).headOption
  }

  property("docUri") = forAll { elem: E =>
    elem.docUri == elem.docUriOption.getOrElse(emptyUri)
  }

  property("baseUri") = forAll { elem: E =>
    elem.baseUri == elem.baseUriOption.getOrElse(emptyUri)
  }

  property("baseUriOption") = forAll { elem: E =>
    elem.baseUriOption == {
      // Recursive call
      val parentBaseUriOption: Option[URI] =
        elem.findParentElem(_ => true).flatMap(_.baseUriOption).orElse(elem.docUriOption)

      elem.attrOption(XmlBaseEName).flatMap(u => parentBaseUriOption.map(_.resolve(u))).orElse(parentBaseUriOption)
    }
  }

  property("rootElem") = forAll { elem: E =>
    elem.rootElem == elem.findAncestorElemOrSelf(_.findParentElem(_ => true).isEmpty).get
  }

  // Other properties

  property("filterAncestorElems-in-terms-of-filter") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterAncestorElems(pred) == elem.filterAncestorElems(_ => true).filter(pred)
  }

  property("filterAncestorElemsOrSelf-in-terms-of-filter") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterAncestorElemsOrSelf(pred) == elem.filterAncestorElemsOrSelf(_ => true).filter(pred)
  }

  property("findTopmostElems-in-terms-of-findAncestorElem") = forAll { (elem: E, pred: E => Boolean) =>
    (elem.findParentElem(_ => true).isEmpty) ==>
      (elem.findTopmostElems(pred) ==
        elem.filterDescendantElems(pred).filter(_.findAncestorElem(e => pred(e) && e != elem).isEmpty))
  }

  property("findTopmostElemsOrSelf-in-terms-of-findAncestorElem") = forAll { (elem: E, pred: E => Boolean) =>
    (elem.findParentElem(_ => true).isEmpty) ==>
      (elem.findTopmostElemsOrSelf(pred) ==
        elem.filterDescendantElemsOrSelf(pred).filter(_.findAncestorElem(pred).isEmpty))
  }

  property("filterAncestorElemsOrSelf-in-terms-of-filterAncestorElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterAncestorElemsOrSelf(pred) ==
      Seq(elem).filter(pred) ++ elem.filterAncestorElems(pred)
  }

  property("filterAncestorElems-in-terms-of-filterAncestorElemsOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterAncestorElems(pred) ==
      elem.filterAncestorElemsOrSelf(pred).filterNot(Set(elem))
  }

  property("parent-of-child-is-this") = forAll { (elem: E, pred: E => Boolean) =>
    (elem.filterChildElems(pred).nonEmpty) ==>
      (elem.filterChildElems(pred).flatMap(_.findParentElem(_ => true)).distinct == Seq(elem))
  }

  property("an-ancestor-of-descendant-is-this") = forAll { (elem: E, pred: E => Boolean) =>
    (elem.filterDescendantElems(pred).nonEmpty) ==>
      (elem.filterDescendantElems(pred).flatMap(_.findAncestorElem(_ == elem)).distinct == Seq(elem))
  }

  property("an-ancestor-or-self-of-descendant-or-self-is-this") = forAll { elem: E =>
    elem.filterDescendantElemsOrSelf(_ => true).flatMap(_.findAncestorElemOrSelf(_ == elem)).distinct == Seq(elem)
  }

  property("select-parent") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(parentElem(pred)) == elem.findParentElem(pred).toSeq
  }

  property("select-ancestor") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(ancestorElems(pred)) == elem.filterAncestorElems(pred)
  }

  property("select-ancestor-or-self") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(ancestorElemsOrSelf(pred)) == elem.filterAncestorElemsOrSelf(pred)
  }

  property("select-siblings-or-self") = forAll { elem: E =>
    elem.select(parentElem() / childElems()) == elem.findParentElem(_ => true).toSeq.flatMap(_.filterChildElems(_ => true))
  }

  private val emptyUri = URI.create("")

  private val XmlBaseEName = EName("http://www.w3.org/XML/1998/namespace", "base")
}
