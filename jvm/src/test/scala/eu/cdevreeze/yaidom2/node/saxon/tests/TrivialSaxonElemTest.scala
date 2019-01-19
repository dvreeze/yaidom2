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

package eu.cdevreeze.yaidom2.node.saxon.tests

import java.io.File

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.queryapi.predicates._
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.streams.Predicates._
import net.sf.saxon.s9api.streams.Steps._
import org.scalatest.FunSuite

class TrivialSaxonElemTest extends FunSuite {

  test("testParseAndQueryXml") {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[TrivialSaxonElemTest].getResource("/test-xml/sample-xbrl-instance.xml").toURI)
    val doc = docBuilder.build(file)

    val saxonRootElem = SaxonNodes.Elem(doc.select(child(isElement)).findFirst().get)

    assertResult(true) {
      saxonRootElem.filterDescendantElemsOrSelf(_ => true).size >= 100
    }

    val knownXbrliLocalNames =
      Set("xbrl", "context", "unit", "entity", "identifier", "segment", "period", "instant", "startDate", "endDate", "measure")

    assertResult(knownXbrliLocalNames) {
      saxonRootElem.filterDescendantElemsOrSelf(_.name.namespaceUriOption.contains(XbrliNs)).map(_.localName).toSet
    }

    assertResult(knownXbrliLocalNames.diff(Set("xbrl"))) {
      saxonRootElem.filterDescendantElems(_.name.namespaceUriOption.contains(XbrliNs)).map(_.localName).toSet
    }

    assertResult(Set(EName(XbrldiNs, "explicitMember"))) {
      saxonRootElem.filterDescendantElemsOrSelf(_.attrOption("dimension").nonEmpty).map(_.name).toSet
    }

    assertResult(Set(GaapNs)) {
      saxonRootElem.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))
        .map(_.attrAsResolvedQName(None, "dimension")).map(_.namespaceUriOption.getOrElse("")).toSet
    }

    assertResult(true) {
      saxonRootElem.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))
        .forall(_.findAncestorElem(havingName(XbrliNs, "context")).nonEmpty)
    }

    assertResult(true) {
      saxonRootElem.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))
        .forall(e => havingName(XbrliNs, "xbrl")(e.rootElem))
    }
  }

  private val XbrliNs = "http://www.xbrl.org/2003/instance"
  private val XbrldiNs = "http://xbrl.org/2006/xbrldi"
  private val GaapNs = "http://xasb.org/gaap"
}
