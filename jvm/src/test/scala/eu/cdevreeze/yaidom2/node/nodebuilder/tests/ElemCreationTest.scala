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

package eu.cdevreeze.yaidom2.node.nodebuilder.tests

import java.io.File

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.core.SimpleScope
import eu.cdevreeze.yaidom2.node.nodebuilder
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.node.simple.SimpleNodes
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.named
import net.sf.saxon.s9api.Processor
import org.scalatest.FunSuite

class ElemCreationTest extends FunSuite {

  private val processor = new Processor(false)

  private def inputXmlFileOnClasspath: String = "/test-xml/sample-xbrl-instance.xml"

  private def saxonDocument: saxon.Document = {
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[XbrlNodeBuildersElemQueryTest].getResource(inputXmlFileOnClasspath).toURI)
    val doc = docBuilder.build(file)

    saxon.Document(doc)
  }

  test("testCreationAndEquivalenceOfXbrlContext") {
    val startScope = SimpleScope.from(
      "xbrli" -> XbrliNs,
      "link" -> LinkNs,
      "xlink" -> XLinkNs,
      "xbrldi" -> XbrldiNs,
      "iso4217" -> Iso4217Ns
    )

    val elemCreator = nodebuilder.NodeBuilders.ElemCreator(startScope)

    def createExplicitMemberElem(dimension: EName, member: EName): nodebuilder.Elem = {
      val currElemCreator =
        elemCreator.appendAggressively(SimpleScope.from("xbrli" -> XbrliNs, "xbrldi" -> XbrldiNs, "gaap" -> GaapNs))

      currElemCreator.textElem(EName(XbrldiNs, "explicitMember"), currElemCreator.simpleScope.findQName(member).get.toString)
        .plusAttribute(EName.fromLocalName("dimension"), currElemCreator.simpleScope.findQName(dimension).get.toString)
    }

    val xbrliEntity: nodebuilder.Elem =
      elemCreator.emptyElem(EName(XbrliNs, "entity"))
        .plusChild(
          elemCreator.textElem(EName(XbrliNs, "identifier"), "1234567890")
            .plusAttribute(EName.fromLocalName("scheme"), "http://www.sec.gov/CIK"))
        .plusChild(elemCreator.emptyElem(EName(XbrliNs, "segment")))
        .transformDescendantElems {
          case e@nodebuilder.Elem(EName(Some(XbrliNs), "segment"), _, _, _) =>
            e.plusChild(createExplicitMemberElem(EName(GaapNs, "EntityAxis"), EName(GaapNs, "ABCCompanyDomain")))
              .plusChild(createExplicitMemberElem(EName(GaapNs, "BusinessSegmentAxis"), EName(GaapNs, "ConsolidatedGroupDomain")))
              .plusChild(createExplicitMemberElem(EName(GaapNs, "VerificationAxis"), EName(GaapNs, "UnqualifiedOpinionMember")))
              .plusChild(createExplicitMemberElem(EName(GaapNs, "PremiseAxis"), EName(GaapNs, "ActualMember")))
              .plusChild(createExplicitMemberElem(EName(GaapNs, "ReportDateAxis"), EName(GaapNs, "ReportedAsOfMarch182008Member")))
          case e => e
        }

    val xbrliPeriod: nodebuilder.Elem =
      elemCreator.emptyElem(EName(XbrliNs, "period"))
        .plusChild(elemCreator.textElem(EName(XbrliNs, "instant"), "2005-12-31"))

    val xbrliContext: nodebuilder.Elem =
      elemCreator.emptyElem(EName(XbrliNs, "context"))
        .plusAttribute(EName.fromLocalName("id"), "I-2005")
        .plusChild(xbrliEntity)
        .plusChild(xbrliPeriod)

    val originalContext: SaxonNodes.Elem =
      saxonDocument.documentElement.findChildElem { e =>
        e.name == EName(XbrliNs, "context") && e.attr("id") == "I-2005"
      }.get.ensuring(_.qname.prefixOption.isEmpty)

    // The original context may use the default namespace for the xbrli namespace, but equality is not affected

    assertResult(resolved.Elem.from(originalContext).removeAllInterElementWhitespace.coalesceAndNormalizeAllText) {
      resolved.Elem.from(xbrliContext).removeAllInterElementWhitespace.coalesceAndNormalizeAllText
    }
  }

  test("testCreationAndEquivalenceOfXbrlInstance") {
    val startScope = SimpleScope.from(
      "xbrli" -> XbrliNs,
      "link" -> LinkNs,
      "xlink" -> XLinkNs,
      "xbrldi" -> XbrldiNs,
      "iso4217" -> Iso4217Ns
    )

    val elemCreator = nodebuilder.NodeBuilders.ElemCreator(startScope)

    val schemaRef: nodebuilder.Elem =
      elemCreator.emptyElem(
        EName(LinkNs, "schemaRef"),
        SeqMap(EName(XLinkNs, "type") -> "simple", EName(XLinkNs, "href") -> "gaap.xsd"))

    val linkbaseRef: nodebuilder.Elem =
      elemCreator.emptyElem(
        EName(LinkNs, "linkbaseRef"),
        SeqMap(
          EName(XLinkNs, "type") -> "simple",
          EName(XLinkNs, "href") -> "gaap-formula.xml",
          EName(XLinkNs, "arcrole") -> "http://www.w3.org/1999/xlink/properties/linkbase"))

    val contexts: Seq[nodebuilder.Elem] = saxonDocument.documentElement.filterChildElems(named(XbrliNs, "context"))
      .map(e => createContext(e))

    val units: Seq[nodebuilder.Elem] = saxonDocument.documentElement.filterChildElems(named(XbrliNs, "unit"))
      .map(e => createUnit(e))

    val facts: Seq[nodebuilder.Elem] = saxonDocument.documentElement.filterChildElems(canBeFact)
      .map(e => createFact(e, elemCreator.simpleScope))

    val footnoteLinks: Seq[nodebuilder.Elem] = saxonDocument.documentElement.filterChildElems(named(LinkNs, "footnoteLink"))
      .map(e => createFootnoteLink(e))

    val xbrlInstance: nodebuilder.Elem =
      elemCreator.emptyElem(EName(XbrliNs, "xbrl"))
        .plusChild(schemaRef)
        .plusChild(linkbaseRef)
        .plusChildren(contexts)
        .plusChildren(units)
        .plusChildren(facts)
        .plusChildren(footnoteLinks)

    def transformElementTree(rootElem: resolved.Elem): resolved.Elem = {
      rootElem.transformDescendantElemsOrSelf {
        case e@resolved.Elem(EName(Some(XbrliNs), "xbrl"), _, _) =>
          e.minusAttribute(EName("http://www.w3.org/2001/XMLSchema-instance", "schemaLocation"))
        case e@resolved.Elem(EName(Some(XbrliNs), "measure"), _, _) =>
          e.withChildren(Seq(resolved.Text(QName.parse(e.text).localPart)))
        case e => e
      }
    }

    val resolvedOriginalRootElem = transformElementTree(resolved.Elem.from(saxonDocument.documentElement))
      .removeAllInterElementWhitespace.coalesceAndNormalizeAllText

    val resolvedCreatedRootElem = transformElementTree(resolved.Elem.from(xbrlInstance))
      .removeAllInterElementWhitespace.coalesceAndNormalizeAllText

    assertResult(resolvedOriginalRootElem.filterChildElems(named(LinkNs, "schemaRef"))) {
      resolvedCreatedRootElem.filterChildElems(named(LinkNs, "schemaRef"))
    }

    assertResult(resolvedOriginalRootElem.filterChildElems(named(LinkNs, "linkbaseRef"))) {
      resolvedCreatedRootElem.filterChildElems(named(LinkNs, "linkbaseRef"))
    }

    assertResult(resolvedOriginalRootElem.filterChildElems(named(XbrliNs, "context"))) {
      resolvedCreatedRootElem.filterChildElems(named(XbrliNs, "context"))
    }

    assertResult(resolvedOriginalRootElem.filterChildElems(named(XbrliNs, "unit"))) {
      resolvedCreatedRootElem.filterChildElems(named(XbrliNs, "unit"))
    }

    assertResult(resolvedOriginalRootElem.filterChildElems(canBeFact)) {
      resolvedCreatedRootElem.filterChildElems(canBeFact)
    }

    assertResult(resolvedOriginalRootElem.filterChildElems(named(LinkNs, "footnoteLink"))) {
      resolvedCreatedRootElem.filterChildElems(named(LinkNs, "footnoteLink"))
    }

    // The real test

    assertResult(resolvedOriginalRootElem) {
      resolvedCreatedRootElem
    }
  }

  test("testCopyingAndEquivalenceOfXbrlInstance") {
    val originalScope = saxonDocument.documentElement.scope.ensuring(_.defaultNamespaceOption.contains(XbrliNs))
    require(saxonDocument.documentElement.findAllDescendantElems.forall(_.scope == originalScope))

    val targetSimpleScope = SimpleScope.from(originalScope.withoutDefaultNamespace)
      .appendAggressively(SimpleScope.from("xbrli" -> XbrliNs))

    val originalRootElemWithoutDefaultNamespace: SimpleNodes.Elem =
      SimpleNodes.Elem.from(saxonDocument.documentElement)
      .transformDescendantElemsOrSelf {
        case e if e.name == EName(XbrliNs, "measure") =>
          val text = targetSimpleScope.findQName(e.textAsResolvedQName).get.toString
          val textNode = SimpleNodes.Text(text, false)
          new SimpleNodes.Elem(QName("xbrli", e.name.localPart), e.attributesByQName, targetSimpleScope.scope, ArraySeq(textNode))
        case e if e.name.namespaceUriOption.contains(XbrliNs) =>
          new SimpleNodes.Elem(QName("xbrli", e.name.localPart), e.attributesByQName, targetSimpleScope.scope, e.children)
        case e =>
          new SimpleNodes.Elem(e.qname, e.attributesByQName, targetSimpleScope.scope, e.children)
      }

    require(originalRootElemWithoutDefaultNamespace.findAllDescendantElemsOrSelf.forall(_.scope == targetSimpleScope.scope))

    val rootElemWithENameContent: SimpleNodes.Elem =
      originalRootElemWithoutDefaultNamespace.transformDescendantElemsOrSelf {
        case e if e.name == EName(XbrldiNs, "explicitMember") =>
          val dimension: EName = e.attrAsResolvedQName(EName.fromLocalName("dimension"))

          e.withAttributesByQName(e.attributesByQName + (QName.fromLocalName("dimension") -> dimension.toString))
            .withChildren(ArraySeq(SimpleNodes.Text(e.textAsResolvedQName.toString, false)))
        case e if e.name == EName(XbrliNs, "measure") =>
          e.withChildren(ArraySeq(SimpleNodes.Text(e.textAsResolvedQName.toString, false)))
        case e => e
      }

    require(rootElemWithENameContent.findAllDescendantElemsOrSelf.forall(_.scope == targetSimpleScope.scope))

    val copiedRootElemWithENameContent: nodebuilder.Elem = nodebuilder.Elem.from(rootElemWithENameContent)

    assertResult(resolved.Elem.from(rootElemWithENameContent).removeAllInterElementWhitespace.coalesceAndNormalizeAllText) {
      resolved.Elem.from(copiedRootElemWithENameContent).removeAllInterElementWhitespace.coalesceAndNormalizeAllText
    }
  }

  private def createContext(originalContext: ScopedNodes.Elem): nodebuilder.Elem = {
    require(originalContext.name == EName(XbrliNs, "context"))
    require(originalContext.scope.defaultNamespaceOption.contains(XbrliNs), s"Expected default namespace $XbrliNs in original")

    val simpleContext = SimpleNodes.Elem.from(originalContext)

    def adaptedScope(e: ScopedNodes.Elem): Scope = {
      e.scope.withoutDefaultNamespace.append(Scope.from("xbrli" -> XbrliNs))
    }

    val transformedSimpleContext: SimpleNodes.Elem =
      simpleContext.transformDescendantElemsOrSelf {
        case e if e.name.namespaceUriOption.contains(XbrliNs) =>
          new SimpleNodes.Elem(QName("xbrli", e.localName), e.attributesByQName, adaptedScope(e), e.children)
        case e =>
          new SimpleNodes.Elem(e.qname, e.attributesByQName, adaptedScope(e), e.children)
      }

    nodebuilder.Elem.from(transformedSimpleContext)
  }

  private def createUnit(originalUnit: ScopedNodes.Elem): nodebuilder.Elem = {
    require(originalUnit.name == EName(XbrliNs, "unit"))
    require(originalUnit.scope.defaultNamespaceOption.contains(XbrliNs), s"Expected default namespace $XbrliNs in original")

    val simpleUnit = SimpleNodes.Elem.from(originalUnit)

    def adaptedScope(e: ScopedNodes.Elem): Scope = {
      e.scope.withoutDefaultNamespace.append(Scope.from("xbrli" -> XbrliNs))
    }

    val transformedSimpleUnit: SimpleNodes.Elem =
      simpleUnit.transformDescendantElemsOrSelf {
        case e if e.name == EName(XbrliNs, "measure") =>
          val measureEName = e.textAsResolvedQName
          val measureQName = SimpleScope.from(adaptedScope(e)).findQName(measureEName).get

          new SimpleNodes.Elem(QName("xbrli", e.localName), e.attributesByQName, adaptedScope(e), ArraySeq(SimpleNodes.Text(measureQName.toString, false)))
        case e if e.name.namespaceUriOption.contains(XbrliNs) =>
          new SimpleNodes.Elem(QName("xbrli", e.localName), e.attributesByQName, adaptedScope(e), e.children)
        case e =>
          new SimpleNodes.Elem(e.qname, e.attributesByQName, adaptedScope(e), e.children)
      }

    nodebuilder.Elem.from(transformedSimpleUnit)
  }

  private def canBeFact(e: ClarkNodes.Elem): Boolean = {
    !Set(XbrliNs, LinkNs).contains(e.name.namespaceUriOption.getOrElse(""))
  }

  private def createFact(originalFact: ScopedNodes.Elem, parentScope: SimpleScope): nodebuilder.Elem = {
    require(canBeFact(originalFact), s"Expected fact")
    require(originalFact.attrOption(EName.fromLocalName("contextRef")).nonEmpty, s"Expected contextRef attribute")
    require(originalFact.qname.prefixOption.contains("gaap"), s"Expected 'gaap' fact")

    require(
      originalFact.attributes.keySet.subsetOf(Set("contextRef", "unitRef", "decimals", "id").map(EName.fromLocalName)),
      s"Unexpected attributes in: ${originalFact.name}")

    val scope = parentScope.appendAggressively(SimpleScope.from("gaap" -> GaapNs))
    val elemCreator = nodebuilder.NodeBuilders.ElemCreator(scope)

    elemCreator.textElem(originalFact.name, originalFact.text)
      .plusAttribute(EName.fromLocalName("contextRef"), originalFact.attr("contextRef"))
      .plusAttributeOption(EName.fromLocalName("unitRef"), originalFact.attrOption("unitRef"))
      .plusAttributeOption(EName.fromLocalName("decimals"), originalFact.attrOption("decimals"))
      .plusAttributeOption(EName.fromLocalName("id"), originalFact.attrOption("id"))
  }

  private def createFootnoteLink(originalFootnoteLink: ScopedNodes.Elem): nodebuilder.Elem = {
    require(originalFootnoteLink.name == EName(LinkNs, "footnoteLink"))
    require(originalFootnoteLink.scope.defaultNamespaceOption.contains(XbrliNs), s"Expected default namespace $XbrliNs in original")

    val simpleFootnoteLink = SimpleNodes.Elem.from(originalFootnoteLink)

    def adaptedScope(e: ScopedNodes.Elem): Scope = {
      e.scope.withoutDefaultNamespace.append(Scope.from("xbrli" -> XbrliNs))
    }

    val transformedSimpleFootnoteLink: SimpleNodes.Elem =
      simpleFootnoteLink.transformDescendantElemsOrSelf {
        case e =>
          new SimpleNodes.Elem(e.qname, e.attributesByQName, adaptedScope(e), e.children)
      }

    nodebuilder.Elem.from(transformedSimpleFootnoteLink)
  }

  private val XbrliNs = "http://www.xbrl.org/2003/instance"
  private val LinkNs = "http://www.xbrl.org/2003/linkbase"
  private val XLinkNs = "http://www.w3.org/1999/xlink"
  private val XbrldiNs = "http://xbrl.org/2006/xbrldi"
  private val Iso4217Ns = "http://www.xbrl.org/2003/iso4217"

  private val GaapNs = "http://xasb.org/gaap"
}
