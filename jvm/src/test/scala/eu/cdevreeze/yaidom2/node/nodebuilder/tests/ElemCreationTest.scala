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
import eu.cdevreeze.yaidom2.node.nodebuilder
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.node.simple.SimpleNodes
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.named
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ListMap

class ElemCreationTest extends AnyFunSuite {

  private val processor = new Processor(false)

  private def inputXmlFileOnClasspath: String = "/test-xml/sample-xbrl-instance.xml"

  private def saxonDocument: saxon.Document = {
    val file = new File(classOf[XbrlNodeBuildersElemQueryTest].getResource(inputXmlFileOnClasspath).toURI)
    saxon.Document.parse(file, processor)
  }

  private val XbrliNs = "http://www.xbrl.org/2003/instance"
  private val LinkNs = "http://www.xbrl.org/2003/linkbase"
  private val XLinkNs = "http://www.w3.org/1999/xlink"
  private val XbrldiNs = "http://xbrl.org/2006/xbrldi"
  private val Iso4217Ns = "http://www.xbrl.org/2003/iso4217"

  private val GaapNs = "http://xasb.org/gaap"

  private val mappings: ListMap[String, String] = ListMap(
    XbrliNs -> "xbrli",
    LinkNs -> "link",
    XLinkNs -> "xlink",
    XbrldiNs -> "xbrldi",
    Iso4217Ns -> "iso4217",
    GaapNs -> "gaap",
  )

  implicit private val namespacePrefixMapper: NamespacePrefixMapper = NamespacePrefixMapper.fromMapWithFallback(mappings)

  implicit private val elemCreator: nodebuilder.NodeBuilderCreator = nodebuilder.NodeBuilderCreator(namespacePrefixMapper)

  import elemCreator._
  import nodebuilder.NodeBuilderCreator._

  test("testCreationAndEquivalenceOfXbrlContext") {
    val prefixedScopeUtil = new PrefixedScopeUtil(namespacePrefixMapper)

    def createExplicitMemberElem(dimension: EName, member: EName, parentScope: PrefixedScope): nodebuilder.Elem = {
      val scope: PrefixedScope = prefixedScopeUtil.extractScope(Seq(dimension, member), parentScope)

      textElem(EName(XbrldiNs, "explicitMember"), scope.findQName(member).get.toString, scope).creationApi
        .plusAttribute(EName.fromLocalName("dimension"), scope.findQName(dimension).get.toString)
        .underlyingElem
    }

    val xbrliEntity: nodebuilder.Elem = {
      emptyElem(EName(XbrliNs, "entity"), PrefixedScope.empty).creationApi
        .plusChild { e =>
          textElem(EName(XbrliNs, "identifier"), "1234567890", e.prefixedScope).creationApi
            .plusAttribute(EName.fromLocalName("scheme"), "http://www.sec.gov/CIK")
            .underlyingElem
        }
        .plusChild(e => emptyElem(EName(XbrliNs, "segment"), e.prefixedScope))
        .underlyingElem
        .transformDescendantElems {
          case e @ nodebuilder.Elem(EName(Some(XbrliNs), "segment"), _, _, _) =>
            e.creationApi
              .plusChild(e => createExplicitMemberElem(EName(GaapNs, "EntityAxis"), EName(GaapNs, "ABCCompanyDomain"), e.prefixedScope))
              .plusChild(e =>
                createExplicitMemberElem(EName(GaapNs, "BusinessSegmentAxis"), EName(GaapNs, "ConsolidatedGroupDomain"), e.prefixedScope))
              .plusChild(e =>
                createExplicitMemberElem(EName(GaapNs, "VerificationAxis"), EName(GaapNs, "UnqualifiedOpinionMember"), e.prefixedScope))
              .plusChild(e => createExplicitMemberElem(EName(GaapNs, "PremiseAxis"), EName(GaapNs, "ActualMember"), e.prefixedScope))
              .plusChild(e =>
                createExplicitMemberElem(EName(GaapNs, "ReportDateAxis"), EName(GaapNs, "ReportedAsOfMarch182008Member"), e.prefixedScope))
              .underlyingElem
          case e => e
        }
    }

    val xbrliPeriod: nodebuilder.Elem =
      emptyElem(EName(XbrliNs, "period"), PrefixedScope.empty).creationApi
        .plusChild(e => textElem(EName(XbrliNs, "instant"), "2005-12-31", e.prefixedScope))
        .underlyingElem

    val xbrliContext: nodebuilder.Elem =
      emptyElem(EName(XbrliNs, "context"), PrefixedScope.empty).creationApi
        .plusAttribute(EName.fromLocalName("id"), "I-2005")
        .plusChild(xbrliEntity)
        .plusChild(xbrliPeriod)
        .underlyingElem

    val originalContext: SaxonNodes.Elem =
      saxonDocument.documentElement
        .findChildElem { e =>
          e.name == EName(XbrliNs, "context") && e.attr("id") == "I-2005"
        }
        .get
        .ensuring(_.qname.prefixOption.isEmpty)

    // The original context may use the default namespace for the xbrli namespace, but equality is not affected

    assertResult(resolved.Elem.from(originalContext).removeAllInterElementWhitespace.coalesceAndNormalizeAllText) {
      resolved.Elem.from(xbrliContext).removeAllInterElementWhitespace.coalesceAndNormalizeAllText
    }
  }

  test("testCreationAndEquivalenceOfXbrlInstance") {
    val schemaRef: nodebuilder.Elem =
      emptyElem(
        EName(LinkNs, "schemaRef"),
        ListMap(EName(XLinkNs, "type") -> "simple", EName(XLinkNs, "href") -> "gaap.xsd"),
        PrefixedScope.empty)

    val linkbaseRef: nodebuilder.Elem =
      emptyElem(
        EName(LinkNs, "linkbaseRef"),
        ListMap(
          EName(XLinkNs, "type") -> "simple",
          EName(XLinkNs, "href") -> "gaap-formula.xml",
          EName(XLinkNs, "arcrole") -> "http://www.w3.org/1999/xlink/properties/linkbase"
        ),
        PrefixedScope.empty
      )

    val contexts: Seq[nodebuilder.Elem] = saxonDocument.documentElement
      .filterChildElems(named(XbrliNs, "context"))
      .map(e => createContext(e))

    val units: Seq[nodebuilder.Elem] = saxonDocument.documentElement
      .filterChildElems(named(XbrliNs, "unit"))
      .map(e => createUnit(e))

    val facts: Seq[nodebuilder.Elem] = saxonDocument.documentElement
      .filterChildElems(canBeFact)
      .map(e => createFact(e, PrefixedScope.from(mappings.map(_.swap))))

    val footnoteLinks: Seq[nodebuilder.Elem] = saxonDocument.documentElement
      .filterChildElems(named(LinkNs, "footnoteLink"))
      .map(e => createFootnoteLink(e))

    val xbrlInstance: nodebuilder.Elem =
      emptyElem(EName(XbrliNs, "xbrl"), PrefixedScope.empty).creationApi
        .plusChild(schemaRef)
        .plusChild(linkbaseRef)
        .plusChildren(contexts)
        .plusChildren(units)
        .plusChildren(facts)
        .plusChildren(footnoteLinks)
        .underlyingElem

    def transformElementTree(rootElem: resolved.Elem): resolved.Elem = {
      implicit val resolvedElemCreator: resolved.ResolvedElemCreator.type = resolved.ResolvedElemCreator
      import resolved.ResolvedElemCreator._

      rootElem.transformDescendantElemsOrSelf {
        case e @ resolved.Elem(EName(Some(XbrliNs), "xbrl"), _, _) =>
          e.creationApi.minusAttribute(EName("http://www.w3.org/2001/XMLSchema-instance", "schemaLocation")).underlying
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

    // The real test

    assertResult(resolvedOriginalRootElem) {
      resolvedCreatedRootElem
    }
  }

  test("testCopyingAndEquivalenceOfXbrlInstance") {
    val originalScope = saxonDocument.documentElement.scope.ensuring(_.defaultNamespaceOption.contains(XbrliNs))
    require(saxonDocument.documentElement.findAllDescendantElems.forall(_.scope == originalScope))

    val targetPrefixedScope = PrefixedScope
      .ignoringDefaultNamespace(originalScope)
      .append(PrefixedScope.from("xbrli" -> XbrliNs))

    val originalRootElemWithoutDefaultNamespace: SimpleNodes.Elem =
      SimpleNodes.Elem
        .from(saxonDocument.documentElement)
        .transformDescendantElemsOrSelf {
          case e if e.name == EName(XbrliNs, "measure") =>
            val text = targetPrefixedScope.findQName(e.textAsResolvedQName).get.toString
            val textNode = SimpleNodes.text(text)
            new SimpleNodes.Elem(QName("xbrli", e.name.localPart), e.attributesByQName, targetPrefixedScope.scope, Vector(textNode))
          case e if e.name.namespaceUriOption.contains(XbrliNs) =>
            new SimpleNodes.Elem(QName("xbrli", e.name.localPart), e.attributesByQName, targetPrefixedScope.scope, e.children)
          case e =>
            new SimpleNodes.Elem(e.qname, e.attributesByQName, targetPrefixedScope.scope, e.children)
        }

    require(originalRootElemWithoutDefaultNamespace.findAllDescendantElemsOrSelf.forall(_.scope == targetPrefixedScope.scope))

    val rootElemWithENameContent: SimpleNodes.Elem =
      originalRootElemWithoutDefaultNamespace.transformDescendantElemsOrSelf {
        case e if e.name == EName(XbrldiNs, "explicitMember") =>
          val dimension: EName = e.attrAsResolvedQName(EName.fromLocalName("dimension"))

          e.withAttributesByQName(e.attributesByQName + (QName.fromLocalName("dimension") -> dimension.toString))
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
          val measureQName = PrefixedScope.from(adaptedScope(e)).findQName(measureEName).get

          new SimpleNodes.Elem(
            QName("xbrli", e.localName),
            e.attributesByQName,
            adaptedScope(e),
            Vector(SimpleNodes.text(measureQName.toString)))
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

  private def createFact(originalFact: ScopedNodes.Elem, parentScope: PrefixedScope): nodebuilder.Elem = {
    require(canBeFact(originalFact), s"Expected fact")
    require(originalFact.attrOption(EName.fromLocalName("contextRef")).nonEmpty, s"Expected contextRef attribute")
    require(originalFact.qname.prefixOption.contains("gaap"), s"Expected 'gaap' fact")

    require(
      originalFact.attributes.keySet.subsetOf(Set("contextRef", "unitRef", "decimals", "id").map(EName.fromLocalName)),
      s"Unexpected attributes in: ${originalFact.name}"
    )

    textElem(originalFact.name, originalFact.text, parentScope).creationApi
      .plusAttribute(EName.fromLocalName("contextRef"), originalFact.attr("contextRef"))
      .plusAttributeOption(EName.fromLocalName("unitRef"), originalFact.attrOption("unitRef"))
      .plusAttributeOption(EName.fromLocalName("decimals"), originalFact.attrOption("decimals"))
      .plusAttributeOption(EName.fromLocalName("id"), originalFact.attrOption("id"))
      .underlyingElem
  }

  private def createFootnoteLink(originalFootnoteLink: ScopedNodes.Elem): nodebuilder.Elem = {
    require(originalFootnoteLink.name == EName(LinkNs, "footnoteLink"))
    require(originalFootnoteLink.scope.defaultNamespaceOption.contains(XbrliNs), s"Expected default namespace $XbrliNs in original")

    val simpleFootnoteLink = SimpleNodes.Elem.from(originalFootnoteLink)

    def adaptedScope(e: ScopedNodes.Elem): Scope = {
      e.scope.withoutDefaultNamespace.append(Scope.from("xbrli" -> XbrliNs))
    }

    val transformedSimpleFootnoteLink: SimpleNodes.Elem =
      simpleFootnoteLink.transformDescendantElemsOrSelf { e =>
        new SimpleNodes.Elem(e.qname, e.attributesByQName, adaptedScope(e), e.children)
      }

    nodebuilder.Elem.from(transformedSimpleFootnoteLink)
  }
}
