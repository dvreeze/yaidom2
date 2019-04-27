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

package eu.cdevreeze.yaidom2.node.indexed.tests

import java.io.File

import scala.collection.immutable.ArraySeq
import scala.jdk.StreamConverters.Ops._

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.indexed.IndexedNodes
import eu.cdevreeze.yaidom2.node.resolved.ResolvedNodes
import eu.cdevreeze.yaidom2.node.saxon.SaxonDocument
import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.node.simple.SimpleNodes
import eu.cdevreeze.yaidom2.queryapi.oo.predicates._
import eu.cdevreeze.yaidom2.queryapi.oo.steps.ElemSteps._
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.streams.Steps._
import org.scalatest.funsuite.AnyFunSuite

class TrivialIndexedElemTest extends AnyFunSuite {

  test("testParseAndQueryXml") {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[TrivialIndexedElemTest].getResource("/test-xml/sample-xbrl-instance.xml").toURI)
    val doc = docBuilder.build(file)

    val rootElem =
      IndexedNodes.Elem.ofRoot(
        Option(doc.getDocumentURI),
        SimpleNodes.Elem.from(SaxonDocument(doc).documentElement))

    assertResult(true) {
      rootElem.findAllDescendantElemsOrSelf().size >= 100
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
      rootElem.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))
        .map(_.attrAsResolvedQName(None, "dimension")).map(_.namespaceUriOption.getOrElse("")).toSet
    }

    assertResult(true) {
      rootElem.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))
        .forall(_.findAncestorElem(havingName(XbrliNs, "context")).nonEmpty)
    }

    assertResult(true) {
      rootElem.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))
        .forall(e => havingName(XbrliNs, "xbrl")(e.rootElem))
    }
  }

  test("testParseAndQueryXmlUsingSteps") {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[TrivialIndexedElemTest].getResource("/test-xml/sample-xbrl-instance.xml").toURI)
    val doc = docBuilder.build(file)

    val rootElem =
      IndexedNodes.Elem.ofRoot(
        Option(doc.getDocumentURI),
        SimpleNodes.Elem.from(SaxonDocument(doc).documentElement))

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
      rootElem.select(descendantElems(havingName(XbrldiNs, "explicitMember")))
        .map(_.attrAsResolvedQName(None, "dimension")).map(_.namespaceUriOption.getOrElse("")).toSet
    }

    assertResult(true) {
      rootElem.select(descendantElems(havingName(XbrldiNs, "explicitMember")))
        .forall(_.select(ancestorElems(havingName(XbrliNs, "context"))).nonEmpty)
    }

    assertResult(true) {
      rootElem.select(descendantElems(havingName(XbrldiNs, "explicitMember")))
        .forall(e => havingName(XbrliNs, "xbrl")(e.rootElem))
    }
  }

  test("testSemanticsOfSteps") {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[TrivialIndexedElemTest].getResource("/test-xml/sample-xbrl-instance.xml").toURI)
    val doc = docBuilder.build(file)

    val rootElem =
      IndexedNodes.Elem.ofRoot(
        Option(doc.getDocumentURI),
        SimpleNodes.Elem.from(SaxonDocument(doc).documentElement))

    val dimensionalContexts =
      rootElem.select {
        descendantElems(XbrliNs, "context").where {
          _.select(childElems(XbrliNs, "entity") / descendantElems(XbrldiNs, "explicitMember")).nonEmpty
        }
      }

    val expectedDimensionalContexts =
      for {
        context <- rootElem.filterDescendantElems(havingName(XbrliNs, "context"))
        if context.filterChildElems(havingName(XbrliNs, "entity"))
          .flatMap(_.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))).nonEmpty
      } yield {
        context
      }

    assertResult(true) {
      dimensionalContexts.size >= 10
    }
    assertResult(expectedDimensionalContexts) {
      dimensionalContexts
    }
  }

  test("testSemanticsOfAncestorSteps") {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[TrivialIndexedElemTest].getResource("/test-xml/sample-xbrl-instance.xml").toURI)
    val doc = docBuilder.build(file)

    val rootElem =
      IndexedNodes.Elem.ofRoot(
        Option(doc.getDocumentURI),
        SimpleNodes.Elem.from(SaxonDocument(doc).documentElement))

    val dimensionalContexts =
      rootElem.select {
        descendantElems(XbrldiNs, "explicitMember") / ancestorElems(XbrliNs, "entity") / parentElem(XbrliNs, "context")
      }.distinct

    val expectedDimensionalContexts =
      (for {
        member <- rootElem.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))
        entity <- member.filterAncestorElems(havingName(XbrliNs, "entity"))
        context <- entity.findParentElem(havingName(XbrliNs, "context"))
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
        context <- rootElem.filterDescendantElems(havingName(XbrliNs, "context"))
        if context.filterChildElems(havingName(XbrliNs, "entity"))
          .flatMap(_.filterDescendantElems(havingName(XbrldiNs, "explicitMember"))).nonEmpty
      } yield {
        context
      }

    assertResult(true) {
      dimensionalContexts.size >= 10
    }
    assertResult(expectedDimensionalContexts) {
      dimensionalContexts
    }
    assertResult(expectedDimensionalContexts2) {
      dimensionalContexts
    }
    assertResult(expectedDimensionalContexts3) {
      dimensionalContexts
    }
  }

  test("testSemanticsOfStepsAgainstSaxon") {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[TrivialIndexedElemTest].getResource("/test-xml/sample-xbrl-instance.xml").toURI)
    val doc = docBuilder.build(file)

    val saxonRootElem = SaxonDocument(doc).documentElement
    val rootElem =
      IndexedNodes.Elem.ofRoot(
        Option(doc.getDocumentURI),
        SimpleNodes.Elem.from(saxonRootElem))

    val dimensionalContexts =
      rootElem.select {
        descendantElems(XbrliNs, "context").where {
          _.select(childElems(XbrliNs, "entity") / descendantElems(XbrldiNs, "explicitMember")).nonEmpty
        }
      }

    val expectedDimensionalContexts =
      saxonRootElem.xdmNode.select {
        descendant(XbrliNs, "context").where {
          (e: XdmNode) => e.select(child(XbrliNs, "entity").`then`(descendant(XbrldiNs, "explicitMember"))).exists
        }
      }.toScala(ArraySeq).map(n => SaxonNodes.Elem(n))

    assertResult(true) {
      dimensionalContexts.size >= 10
    }
    assertResult(expectedDimensionalContexts.map(e => ResolvedNodes.Elem.from(e))) {
      dimensionalContexts.map(e => ResolvedNodes.Elem.from(e))
    }
  }

  private val XbrliNs = "http://www.xbrl.org/2003/instance"
  private val XbrldiNs = "http://xbrl.org/2006/xbrldi"
  private val GaapNs = "http://xasb.org/gaap"
}
