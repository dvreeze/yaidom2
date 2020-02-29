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

package eu.cdevreeze.yaidom2.queryapi.tests

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi.BackingElemStepFactoryApi
import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import eu.cdevreeze.yaidom2.queryapi._

abstract class XbrlBackingElemQueryTest[E <: BackingNodes.Elem.Aux[_, E]] extends XbrlScopedElemQueryTest[E] {

  // Stable identifier, needed for the import below. Because of this val, an abstract class is used rather than a trait.
  // Given that concrete subclasses of this abstract class typically have no value equality defined, this is no problem.
  // In this case, this abstract class does not even introduce any new "state", so this is even less of a problem here.

  protected val elemStepFactory: BackingElemStepFactoryApi.Aux[E]

  import elemStepFactory._

  test("testParseAndQueryXml") {
    assertResult(true) {
      rootElem.findAllDescendantElemsOrSelf.size >= 100
    }

    val knownXbrliLocalNames =
      Set("xbrl", "context", "unit", "entity", "identifier", "segment", "period", "instant", "startDate", "endDate", "measure")

    assertResult(knownXbrliLocalNames) {
      rootElem.filterDescendantElemsOrSelf(_.name.namespaceUriOption.contains(XbrliNs)).map(_.localName).toSet
    }

    assertResult(knownXbrliLocalNames.diff(Set("xbrl"))) {
      rootElem.filterDescendantElems(_.name.namespaceUriOption.contains(XbrliNs)).map(_.localName).toSet
    }

    assertResult(Set(EName(XbrldiNs, "explicitMember"))) {
      rootElem.filterDescendantElemsOrSelf(_.attrOption("dimension").nonEmpty).map(_.name).toSet
    }

    assertResult(Set(GaapNs)) {
      rootElem.filterDescendantElems(named(XbrldiNs, "explicitMember"))
        .map(_.attrAsResolvedQName(None, "dimension")).map(_.namespaceUriOption.getOrElse("")).toSet
    }

    assertResult(true) {
      rootElem.filterDescendantElems(named(XbrldiNs, "explicitMember"))
        .forall(_.findAncestorElem(named(XbrliNs, "context")).nonEmpty)
    }

    assertResult(true) {
      rootElem.filterDescendantElems(named(XbrldiNs, "explicitMember"))
        .forall(e => named(XbrliNs, "xbrl")(e.rootElem))
    }
  }

  test("testParseAndQueryXmlUsingSteps") {
    assertResult(true) {
      rootElem.select(descendantElemsOrSelf()).size >= 100
    }

    val knownXbrliLocalNames =
      Set("xbrl", "context", "unit", "entity", "identifier", "segment", "period", "instant", "startDate", "endDate", "measure")

    assertResult(knownXbrliLocalNames) {
      rootElem.select(descendantElemsOrSelf(_.name.namespaceUriOption.contains(XbrliNs))).map(_.localName).toSet
    }

    assertResult(knownXbrliLocalNames.diff(Set("xbrl"))) {
      rootElem.select(descendantElems(_.name.namespaceUriOption.contains(XbrliNs))).map(_.localName).toSet
    }

    assertResult(Set(EName(XbrldiNs, "explicitMember"))) {
      rootElem.select(descendantElemsOrSelf(_.attrOption("dimension").nonEmpty)).map(_.name).toSet
    }

    assertResult(Set(GaapNs)) {
      rootElem.select(descendantElems(named(XbrldiNs, "explicitMember")))
        .map(_.attrAsResolvedQName(None, "dimension")).map(_.namespaceUriOption.getOrElse("")).toSet
    }

    assertResult(true) {
      rootElem.select(descendantElems(named(XbrldiNs, "explicitMember")))
        .forall(_.select(ancestorElems(named(XbrliNs, "context"))).nonEmpty)
    }

    assertResult(true) {
      rootElem.select(descendantElems(named(XbrldiNs, "explicitMember")))
        .forall(e => named(XbrliNs, "xbrl")(e.rootElem))
    }
  }

  test("testSemanticsOfAncestorSteps") {
    val dimensionalContexts =
      rootElem.select {
        descendantElems(XbrldiNs, "explicitMember") / ancestorElems(XbrliNs, "entity") / parentElem(XbrliNs, "context")
      }.distinct

    val expectedDimensionalContexts =
      (for {
        member <- rootElem.filterDescendantElems(named(XbrldiNs, "explicitMember"))
        entity <- member.filterAncestorElems(named(XbrliNs, "entity"))
        context <- entity.findParentElem(named(XbrliNs, "context"))
      } yield {
        context
      }).distinct

    val expectedDimensionalContexts2 =
      rootElem.select {
        descendantElems(XbrliNs, "context").where {
          _.select(childElems(XbrliNs, "entity") / descendantElems(XbrldiNs, "explicitMember")).nonEmpty
        }
      }

    val expectedDimensionalContexts3 =
      for {
        context <- rootElem.filterDescendantElems(named(XbrliNs, "context"))
        if context.filterChildElems(named(XbrliNs, "entity"))
          .flatMap(_.filterDescendantElems(named(XbrldiNs, "explicitMember"))).nonEmpty
      } yield {
        context
      }

    assertResult(true) {
      dimensionalContexts.size >= 10
    }
    assertResult(expectedDimensionalContexts.map(e => resolved.Elem.from(e))) {
      dimensionalContexts.map(e => resolved.Elem.from(e))
    }
    assertResult(expectedDimensionalContexts2.map(e => resolved.Elem.from(e))) {
      dimensionalContexts.map(e => resolved.Elem.from(e))
    }
    assertResult(expectedDimensionalContexts3.map(e => resolved.Elem.from(e))) {
      dimensionalContexts.map(e => resolved.Elem.from(e))
    }
  }
}
