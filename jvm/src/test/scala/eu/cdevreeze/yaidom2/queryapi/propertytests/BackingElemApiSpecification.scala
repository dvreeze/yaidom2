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
import eu.cdevreeze.yaidom2.queryapi.BackingElemStepFactoryApi
import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

trait BackingElemApiSpecification[N, E <: BackingNodes.Elem.Aux[N, E]] extends ScopedElemApiSpecification[N, E] {
  self: Properties =>

  protected def elemStepFactory: BackingElemStepFactoryApi.Aux[E]

  // "Definitions" of BackingElemApi methods

  property("findParentElem") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findParentElem(pred) == elem.findParentElem.filter(pred)
  }

  property("findParentElem") = forAll { (elem: E) =>
    elem.findParentElem == elem.findParentElem(_ => true)
  }

  property("filterAncestorElems") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterAncestorElems(pred) ==
      elem.findParentElem.toList.flatMap(_.filterAncestorElemsOrSelf(pred))
  }

  property("findAllAncestorElems") = forAll { (elem: E) =>
    elem.findAllAncestorElems == elem.filterAncestorElems(_ => true)
  }

  property("filterAncestorElemsOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    // Recursive calls
    elem.filterAncestorElemsOrSelf(pred) ==
      Seq(elem).filter(pred) ++ elem.findParentElem.toList.flatMap(_.filterAncestorElemsOrSelf(pred))
  }

  property("findAllAncestorElemsOrSelf") = forAll { (elem: E) =>
    elem.findAllAncestorElemsOrSelf == elem.filterAncestorElemsOrSelf(_ => true)
  }

  property("findAncestorElem") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findAncestorElem(pred) == elem.filterAncestorElems(pred).headOption
  }

  property("findAncestorElemOrSelf") = forAll { (elem: E, pred: E => Boolean) =>
    elem.findAncestorElemOrSelf(pred) == elem.filterAncestorElemsOrSelf(pred).headOption
  }

  property("findAllPrecedingSiblingElems") = forAll { (elem: E) =>
    elem.findAllPrecedingSiblingElems ==
      elem.findParentElem.toList.flatMap(_.findAllChildElems.takeWhile(_ != elem)).reverse
  }

  property("docUri") = forAll { (elem: E) =>
    elem.docUri == elem.docUriOption.getOrElse(emptyUri)
  }

  property("baseUri") = forAll { (elem: E) =>
    elem.baseUri == elem.baseUriOption.getOrElse(emptyUri)
  }

  property("baseUriOption") = forAll { (elem: E) =>
    elem.baseUriOption == {
      // Recursive call
      val parentBaseUriOption: Option[URI] =
        elem.findParentElem.flatMap(_.baseUriOption).orElse(elem.docUriOption)

      elem.attrOption(XmlBaseEName).map(u => parentBaseUriOption.map(_.resolve(u)).getOrElse(u))
        .orElse(parentBaseUriOption)
    }
  }

  property("rootElem") = forAll { (elem: E) =>
    elem.rootElem == elem.findAncestorElemOrSelf(_.findParentElem.isEmpty).get
  }

  // Other properties

  property("filterAncestorElems-in-terms-of-filter") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterAncestorElems(pred) == elem.findAllAncestorElems.filter(pred)
  }

  property("filterAncestorElemsOrSelf-in-terms-of-filter") = forAll { (elem: E, pred: E => Boolean) =>
    elem.filterAncestorElemsOrSelf(pred) == elem.findAllAncestorElemsOrSelf.filter(pred)
  }

  property("findTopmostElems-in-terms-of-findAncestorElem") = forAll { (elem: E, pred: E => Boolean) =>
    (elem.findParentElem.isEmpty) ==>
      (elem.findTopmostElems(pred) ==
        elem.filterDescendantElems(pred).filter(_.findAncestorElem(e => pred(e) && e != elem).isEmpty))
  }

  property("findTopmostElemsOrSelf-in-terms-of-findAncestorElem") = forAll { (elem: E, pred: E => Boolean) =>
    (elem.findParentElem.isEmpty) ==>
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
      (elem.filterChildElems(pred).flatMap(_.findParentElem).distinct == Seq(elem))
  }

  property("an-ancestor-of-descendant-is-this") = forAll { (elem: E, pred: E => Boolean) =>
    (elem.filterDescendantElems(pred).nonEmpty) ==>
      (elem.filterDescendantElems(pred).flatMap(_.findAncestorElem(_ == elem)).distinct == Seq(elem))
  }

  property("an-ancestor-or-self-of-descendant-or-self-is-this") = forAll { (elem: E) =>
    elem.findAllDescendantElemsOrSelf.flatMap(_.findAncestorElemOrSelf(_ == elem)).distinct == Seq(elem)
  }

  property("select-parent") = forAll { (elem: E, pred: E => Boolean) =>
    val elemSteps = elemStepFactory
    import elemSteps._

    elem.select(parentElem(pred)) == elem.findParentElem(pred).toList
  }

  property("select-ancestor") = forAll { (elem: E, pred: E => Boolean) =>
    val elemSteps = elemStepFactory
    import elemSteps._

    elem.select(ancestorElems(pred)) == elem.filterAncestorElems(pred)
  }

  property("select-ancestor-or-self") = forAll { (elem: E, pred: E => Boolean) =>
    val elemSteps = elemStepFactory
    import elemSteps._

    elem.select(ancestorElemsOrSelf(pred)) == elem.filterAncestorElemsOrSelf(pred)
  }

  property("select-siblings-or-self") = forAll { (elem: E) =>
    val elemSteps = elemStepFactory
    import elemSteps._

    elem.select(parentElem() / childElems()) == elem.findParentElem.toList.flatMap(_.findAllChildElems)
  }

  property("own-navigation-path-followed-from-root-returns-same-element") = forAll { (elem: E) =>
    elem.rootElem.getDescendantElemOrSelf(elem.ownNavigationPathRelativeToRootElem) == elem
  }

  private val emptyUri = URI.create("")

  private val XmlBaseEName = EName("http://www.w3.org/XML/1998/namespace", "base")
}
