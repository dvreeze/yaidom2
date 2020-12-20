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

import eu.cdevreeze.yaidom2.core._
import eu.cdevreeze.yaidom2.node.resolved.ResolvedElemCreator
import eu.cdevreeze.yaidom2.node.nodebuilder
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.node.saxon.SaxonProducers
import eu.cdevreeze.yaidom2.node.simple.SimpleNodes
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.named
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ListMap
import scala.util.chaining._

class ElemCreationTest extends AnyFunSuite {

  private val processor = new Processor(false)

  private def inputXmlFileOnClasspath: String = "/test-xml/sample-xbrl-instance.xml"

  private def saxonDocument: saxon.Document = {
    val file = new File(classOf[XbrlNodeBuildersElemQueryTest].getResource(inputXmlFileOnClasspath).toURI)
    SaxonProducers.parser(processor).parse(file)
  }

  private val XbrliNs = "http://www.xbrl.org/2003/instance"
  private val LinkNs = "http://www.xbrl.org/2003/linkbase"
  private val XLinkNs = "http://www.w3.org/1999/xlink"
  private val XbrldiNs = "http://xbrl.org/2006/xbrldi"
  private val Iso4217Ns = "http://www.xbrl.org/2003/iso4217"

  private val GaapNs = "http://xasb.org/gaap"

  // A "known stable scope" is chosen, with default namespace. The choice of having a default namespace can not
  // be changed anymore when appending scopes (using function appendNonConflictingScope etc.) to this stable scope.
  // This known scope contains the prefixes ommonly found in XBRL instances, but may miss prefixes of facts, dimensions etc.
  // Here it misses the "gaap" namespace prefix.

  private val knownStableScope: StableScope = StableScope.from(
    "" -> XbrliNs,
    "xbrli" -> XbrliNs,
    "link" -> LinkNs,
    "xlink" -> XLinkNs,
    "xbrldi" -> XbrldiNs,
    "iso4217" -> Iso4217Ns,
  )

  // The stable scope containing the specific prefixes in fact names, dimensions, members etc.

  private val extraStableScope: StableScope =
    StableScope.from("gaap" -> GaapNs).ensuring(sc => knownStableScope.canAppendNonConflictingScope(sc))

  private val elemCreator: nodebuilder.NodeBuilderCreator = nodebuilder.NodeBuilderCreator(knownStableScope)

  import elemCreator._

  test("testUseDefaultNamespaceAndPrefixDuringElementCreation") {
    // Using the default namespace (same namespace as for prefix "xbrli")

    val identifier: nodebuilder.ElemInKnownScope =
      textElem(q"identifier", "1234567890")
        .plusAttribute(q"scheme", "http://www.sec.gov/CIK")

    // Here using the "xbrli" prefix instead

    val entity: nodebuilder.ElemInKnownScope = emptyElem(q"xbrli:entity").plusChild(identifier.elem)

    assertResult(knownStableScope.filterKeysCompatibly(Set(""))) {
      identifier.elem.stableScope
    }
    assertResult(knownStableScope.filterKeysCompatibly(Set.empty)) {
      identifier.elem.stableScope
    }

    assertResult(knownStableScope.filterKeysCompatibly(Set("", "xbrli"))) {
      entity.elem.stableScope
    }
    assertResult(knownStableScope.filterKeysCompatibly(Set("xbrli"))) {
      entity.elem.stableScope
    }

    assertResult(knownStableScope.filterKeysCompatibly(Set.empty)) {
      entity.elem
        .findChildElem(named(XbrliNs, "identifier"))
        .map(_.stableScope)
        .getOrElse(StableScope.empty)
    }
    assertResult(knownStableScope.filterKeysCompatibly(Set("xbrli"))) {
      entity
        .usingExtraScope(StableScope.empty) // Prevent the occurrence of any namespace undeclarations
        .elem
        .findChildElem(named(XbrliNs, "identifier"))
        .map(_.stableScope)
        .getOrElse(StableScope.empty)
    }

    val expectedResolvedEntity: resolved.Elem = {
      val resolvedElemCreator: ResolvedElemCreator = ResolvedElemCreator(knownStableScope)

      resolvedElemCreator
        .elem(
          q"xbrli:entity",
          Vector(
            resolvedElemCreator
              .textElem(q"xbrli:identifier", ListMap(q"scheme" -> "http://www.sec.gov/CIK"), "1234567890")
              .elem
          )
        )
        .elem
    }

    assertResult(expectedResolvedEntity) {
      resolved.Elem.from(entity.elem)
    }
  }

  test("testPushUpPrefixedNamespaceDeclarations") {
    val testNs = "http://www.test.com/test/"
    val customScope = StableScope.from("test" -> testNs)

    val identifier: nodebuilder.Elem =
      textElem(q"identifier", "1234567890")
        .plusAttribute(q"scheme", "http://www.sec.gov/CIK")
        .usingExtraScope(customScope)
        .elem

    val entity: nodebuilder.Elem =
      emptyElem(q"xbrli:entity", StableScope.from("xbrli" -> XbrliNs))
        .plusChild(identifier)
        .usingExtraScope(identifier.stableScope)
        .elem

    assertResult(Seq("test")) {
      entity.scope.prefixesForNamespace(testNs)
    }

    val foundIdentifier: nodebuilder.Elem = entity.findChildElem(named(XbrliNs, "identifier")).get

    assertResult(Seq("test")) {
      foundIdentifier.scope.prefixesForNamespace(testNs)
    }

    assertResult(Scope.from("test" -> testNs, "xbrli" -> XbrliNs, "" -> XbrliNs)) {
      foundIdentifier.scope.filterNamespaces(Set(XbrliNs, testNs))
    }
    assertResult(foundIdentifier.scope) {
      entity.scope
    }

    val expectedResolvedEntity: resolved.Elem = {
      val resolvedElemCreator: ResolvedElemCreator = ResolvedElemCreator(knownStableScope)
      import resolvedElemCreator._

      emptyElem(q"xbrli:entity").plusChildElem {
        textElem(q"xbrli:identifier", "1234567890")
          .plusAttribute(q"scheme", "http://www.sec.gov/CIK")
      }.elem
    }

    assertResult(expectedResolvedEntity) {
      resolved.Elem.from(entity)
    }
  }

  test("testCreationAndEquivalenceOfXbrlContext") {
    def createExplicitMemberElem(dimension: QName, member: QName): nodebuilder.Elem = {
      textElem(q"xbrldi:explicitMember", member.toString)
        .usingExtraScope(extraStableScope.filterKeysCompatibly(dimension.prefixOption.toSet))
        .usingExtraScope(extraStableScope.filterKeysCompatibly(member.prefixOption.toSet))
        .plusAttribute(q"dimension", dimension.toString)
        .elem
    }

    val xbrliEntity: nodebuilder.Elem = {
      emptyElem(q"xbrli:entity")
        .plusChildElem {
          textElem(q"xbrli:identifier", "1234567890")
            .plusAttribute(q"scheme", "http://www.sec.gov/CIK")
        }
        .plusChildElem(emptyElem(q"xbrli:segment"))
        .elem
        .transformDescendantElems {
          case e @ nodebuilder.Elem(QName(Some("xbrli"), "segment"), _, _, _) =>
            nodebuilder.ElemInKnownScope
              .from(e, knownStableScope)
              .usingExtraScope(extraStableScope.filterKeysCompatibly(Set("gaap")))
              .plusChild(createExplicitMemberElem(q"gaap:EntityAxis", q"gaap:ABCCompanyDomain"))
              .plusChild(createExplicitMemberElem(q"gaap:BusinessSegmentAxis", q"gaap:ConsolidatedGroupDomain"))
              .plusChild(createExplicitMemberElem(q"gaap:VerificationAxis", q"gaap:UnqualifiedOpinionMember"))
              .plusChild(createExplicitMemberElem(q"gaap:PremiseAxis", q"gaap:ActualMember"))
              .plusChild(createExplicitMemberElem(q"gaap:ReportDateAxis", q"gaap:ReportedAsOfMarch182008Member"))
              .validated
              .elem
          case e => e
        }
    }

    val xbrliPeriod: nodebuilder.Elem =
      emptyElem(q"xbrli:period")
        .plusChildElem(textElem(q"xbrli:instant", "2005-12-31"))
        .elem

    val xbrliContext: nodebuilder.Elem =
      emptyElem(q"xbrli:context")
        .plusAttribute(q"id", "I-2005")
        .plusChild(xbrliEntity)
        .plusChild(xbrliPeriod)
        .usingExtraScope(StableScope.empty)
        .elem

    val originalContext: SaxonNodes.Elem =
      saxonDocument.documentElement
        .findChildElem { e =>
          e.name == EName(XbrliNs, "context") && e.attr("id") == "I-2005"
        }
        .ensuring(_.nonEmpty)
        .get
        .ensuring(_.qname.prefixOption.isEmpty)

    // The original context may use the default namespace for the xbrli namespace, but equality is not affected

    assertResult(resolved.Elem.from(originalContext).removeAllInterElementWhitespace.coalesceAndNormalizeAllText) {
      resolved.Elem.from(xbrliContext).removeAllInterElementWhitespace.coalesceAndNormalizeAllText
    }
  }

  test("testCreationAndEquivalenceOfXbrlInstance") {
    val schemaRef: nodebuilder.Elem =
      emptyElem(q"link:schemaRef", ListMap(q"xlink:type" -> "simple", q"xlink:href" -> "gaap.xsd")).elem

    val linkbaseRef: nodebuilder.Elem =
      emptyElem(
        q"link:linkbaseRef",
        ListMap(
          q"xlink:type" -> "simple",
          q"xlink:href" -> "gaap-formula.xml",
          q"xlink:arcrole" -> "http://www.w3.org/1999/xlink/properties/linkbase"
        )
      ).elem

    val contexts: Seq[nodebuilder.Elem] = saxonDocument.documentElement
      .filterChildElems(named(XbrliNs, "context"))
      .map(e => createContext(e))

    val units: Seq[nodebuilder.Elem] = saxonDocument.documentElement
      .filterChildElems(named(XbrliNs, "unit"))
      .map(e => createUnit(e))

    val facts: Seq[nodebuilder.Elem] = saxonDocument.documentElement
      .filterChildElems(canBeFact)
      .map(e => createFact(e, knownStableScope.appendNonConflictingScope(extraStableScope)))

    val footnoteLinks: Seq[nodebuilder.Elem] = saxonDocument.documentElement
      .filterChildElems(named(LinkNs, "footnoteLink"))
      .map(e => createFootnoteLink(e))

    val xbrlInstance: nodebuilder.Elem =
      emptyElem(q"xbrli:xbrl")
        .plusChild(schemaRef)
        .plusChild(linkbaseRef)
        .plusChildren(contexts)
        .plusChildren(units)
        .plusChildren(facts)
        .plusChildren(footnoteLinks)
        .usingExtraScope(StableScope.empty)
        .elem

    def transformElementTree(rootElem: resolved.Elem): resolved.Elem = {
      rootElem.transformDescendantElemsOrSelf {
        case e @ resolved.Elem(EName(Some(XbrliNs), "xbrl"), _, _) =>
          e.pipe(
              e =>
                resolved
                  .ElemInKnownScope(
                    e,
                    knownStableScope.appendNonConflictingScope(StableScope.from("xsi" -> "http://www.w3.org/2001/XMLSchema-instance"))))
            .minusAttribute(q"xsi:schemaLocation")
            .elem
        case e @ resolved.Elem(EName(Some(XbrliNs), "measure"), _, _) =>
          e.withChildren(Seq(resolved.Text(QName.parse(e.text).localPart)))
        case e => e
      }
    }

    val resolvedOriginalRootElem =
      transformElementTree(resolved.Elem.from(saxonDocument.documentElement)).removeAllInterElementWhitespace.coalesceAndNormalizeAllText

    val resolvedCreatedRootElem =
      transformElementTree(resolved.Elem.from(xbrlInstance)).removeAllInterElementWhitespace.coalesceAndNormalizeAllText

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

    assertResult(resolvedOriginalRootElem.withChildren(Seq.empty)) {
      resolvedCreatedRootElem.withChildren(Seq.empty)
    }

    // The real test

    assertResult(resolvedOriginalRootElem) {
      resolvedCreatedRootElem
    }
  }

  test("testCopyingAndEquivalenceOfXbrlInstance") {
    val originalScope = saxonDocument.documentElement.scope.ensuring(_.defaultNamespaceOption.contains(XbrliNs))
    require(saxonDocument.documentElement.findAllDescendantElems.forall(_.scope == originalScope))

    val targetPrefixedScope = StableScope
      .from(originalScope.withoutDefaultNamespace)
      .appendCompatibleScope(StableScope.from("xbrli" -> XbrliNs))

    val originalRootElemWithoutDefaultNamespace: SimpleNodes.Elem =
      SimpleNodes.Elem
        .from(saxonDocument.documentElement)
        .transformDescendantElemsOrSelf {
          case e if e.name == EName(XbrliNs, "measure") =>
            val text = e.textAsQName.toString
            val textNode = SimpleNodes.text(text)
            new SimpleNodes.Elem(q"xbrli:${e.name.localPart}", e.attributesByQName, targetPrefixedScope.scope, Vector(textNode))
          case e if e.name.namespaceUriOption.contains(XbrliNs) =>
            new SimpleNodes.Elem(q"xbrli:${e.name.localPart}", e.attributesByQName, targetPrefixedScope.scope, e.children)
          case e =>
            new SimpleNodes.Elem(e.qname, e.attributesByQName, targetPrefixedScope.scope, e.children)
        }

    require(originalRootElemWithoutDefaultNamespace.findAllDescendantElemsOrSelf.forall(_.scope == targetPrefixedScope.scope))

    val rootElemWithENameContent: SimpleNodes.Elem =
      originalRootElemWithoutDefaultNamespace.transformDescendantElemsOrSelf {
        case e if e.name == EName(XbrldiNs, "explicitMember") =>
          val dimension: EName = e.attrAsResolvedQName(e"dimension")

          e.withAttributesByQName(e.attributesByQName + (q"dimension" -> dimension.toString))
            .withChildren(Vector(SimpleNodes.text(e.textAsResolvedQName.toString)))
        case e if e.name == EName(XbrliNs, "measure") =>
          e.withChildren(Vector(SimpleNodes.text(e.textAsResolvedQName.toString)))
        case e => e
      }

    require(rootElemWithENameContent.findAllDescendantElemsOrSelf.forall(_.scope == targetPrefixedScope.scope))

    val copiedRootElemWithENameContent: nodebuilder.Elem = nodebuilder.Elem.from(rootElemWithENameContent)

    assertResult(resolved.Elem.from(rootElemWithENameContent).removeAllInterElementWhitespace.coalesceAndNormalizeAllText) {
      resolved.Elem.from(copiedRootElemWithENameContent).removeAllInterElementWhitespace.coalesceAndNormalizeAllText
    }
  }

  private def createContext(originalContext: ScopedNodes.Elem): nodebuilder.Elem = {
    require(originalContext.name == EName(XbrliNs, "context"))
    require(originalContext.scope.defaultNamespaceOption.contains(XbrliNs), s"Expected default namespace $XbrliNs in original")

    val simpleContext = SimpleNodes.Elem.from(originalContext)

    nodebuilder.Elem.from(simpleContext)
  }

  private def createUnit(originalUnit: ScopedNodes.Elem): nodebuilder.Elem = {
    require(originalUnit.name == EName(XbrliNs, "unit"))
    require(originalUnit.scope.defaultNamespaceOption.contains(XbrliNs), s"Expected default namespace $XbrliNs in original")

    val simpleUnit = SimpleNodes.Elem.from(originalUnit)

    nodebuilder.Elem.from(simpleUnit)
  }

  private def canBeFact(e: ClarkNodes.Elem): Boolean = {
    !Set(XbrliNs, LinkNs).contains(e.name.namespaceUriOption.getOrElse(""))
  }

  private def createFact(originalFact: ScopedNodes.Elem, parentScope: StableScope): nodebuilder.Elem = {
    require(canBeFact(originalFact), s"Expected fact")
    require(originalFact.attrOption(e"contextRef").nonEmpty, s"Expected contextRef attribute")
    require(originalFact.qname.prefixOption.contains("gaap"), s"Expected 'gaap' fact")

    require(
      originalFact.attributes.keySet.subsetOf(Set("contextRef", "unitRef", "decimals", "id").map(EName.fromLocalName)),
      s"Unexpected attributes in: ${originalFact.name}"
    )

    nodebuilder
      .NodeBuilderCreator(parentScope)
      .textElem(originalFact.qname, originalFact.text)
      .plusAttribute(q"contextRef", originalFact.attr("contextRef"))
      .plusAttributeOption(q"unitRef", originalFact.attrOption("unitRef"))
      .plusAttributeOption(q"decimals", originalFact.attrOption("decimals"))
      .plusAttributeOption(q"id", originalFact.attrOption("id"))
      .validated
      .elem
  }

  private def createFootnoteLink(originalFootnoteLink: ScopedNodes.Elem): nodebuilder.Elem = {
    require(originalFootnoteLink.name == EName(LinkNs, "footnoteLink"))
    require(originalFootnoteLink.scope.defaultNamespaceOption.contains(XbrliNs), s"Expected default namespace $XbrliNs in original")

    val simpleFootnoteLink = SimpleNodes.Elem.from(originalFootnoteLink)

    nodebuilder.Elem.from(simpleFootnoteLink)
  }
}
