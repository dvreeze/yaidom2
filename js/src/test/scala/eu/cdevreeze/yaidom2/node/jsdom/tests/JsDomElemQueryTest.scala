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

package eu.cdevreeze.yaidom2.node.jsdom.tests

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.indexed
import eu.cdevreeze.yaidom2.node.jsdom.JsDomDocument
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi._
import org.scalajs.dom.experimental.domparser.DOMParser
import org.scalajs.dom.experimental.domparser.SupportedType
import org.scalatest.funsuite.AnyFunSuite

/**
 * Test case testing queries on JS-DOM elements, comparing them to native yaidom element query results.
 *
 * This test case assumes that the element implementations on the JVM have already been tested extensively and can
 * therefore be trusted.
 *
 * @author Chris de Vreeze
 */
class JsDomElemQueryTest extends AnyFunSuite {

  test("testFindAllChildElems") {
    val docElem = axesSchemaDoc.documentElement

    assertResult(true) {
      docElem.findAllChildElems.size > 20
    }
    assertResult(Set(XsNamespace)) {
      docElem.findAllChildElems().map(_.namespaceAsString).toSet
    }

    assertResult(docElem.findAllChildElems()) {
      docElem.filterChildElems(_ => true)
    }

    val indexedDoc = indexed.Document.from(axesSchemaDoc)

    assertResult(resolved.Elem.from(docElem).findAllChildElems()) {
      resolved.Elem.from(indexedDoc.documentElement).findAllChildElems()
    }
  }

  test("testFindAllDescendantElems") {
    val docElem = axesSchemaDoc.documentElement

    assertResult(true) {
      docElem.findAllDescendantElems.size > 20
    }
    assertResult(Set(XsNamespace, LinkNamespace)) {
      docElem.findAllDescendantElems().map(_.namespaceAsString).toSet
    }

    assertResult(docElem.findAllDescendantElems()) {
      docElem.filterDescendantElems(_ => true)
    }

    val indexedDoc = indexed.Document.from(axesSchemaDoc)

    assertResult(resolved.Elem.from(docElem).findAllDescendantElems()) {
      resolved.Elem.from(indexedDoc.documentElement).findAllDescendantElems()
    }
  }

  test("testFindAllDescendantElemsOrSelf") {
    val docElem = axesSchemaDoc.documentElement

    assertResult(true) {
      docElem.findAllDescendantElemsOrSelf.size > 20
    }
    assertResult(Set(XsNamespace, LinkNamespace)) {
      docElem.findAllDescendantElemsOrSelf().map(_.namespaceAsString).toSet
    }

    assertResult(docElem.findAllDescendantElemsOrSelf()) {
      docElem.filterDescendantElemsOrSelf(_ => true)
    }

    assertResult(docElem.findAllDescendantElems().toSet.union(Set(docElem))) {
      docElem.findAllDescendantElemsOrSelf().toSet
    }

    val indexedDoc = indexed.Document.from(axesSchemaDoc)

    assertResult(resolved.Elem.from(docElem).findAllDescendantElemsOrSelf()) {
      resolved.Elem.from(indexedDoc.documentElement).findAllDescendantElemsOrSelf()
    }
  }

  test("testFilterDescendantElemsOrSelf") {
    val docElem = axesSchemaDoc.documentElement

    val abstractElemDecls =
      docElem.filterDescendantElemsOrSelf(named(XsNamespace, "element"))
        .filter(_.attrOption("abstract").getOrElse("true").toBoolean)

    assertResult(true) {
      abstractElemDecls.size > 20
    }

    val nonAbstractElemDecls =
      docElem.filterDescendantElemsOrSelf(named(XsNamespace, "element"))
        .filter(_.attrOption("abstract").getOrElse("true").toBoolean == false)

    assertResult(Seq.empty) {
      nonAbstractElemDecls
    }

    val indexedAbstractElemDecls =
      indexed.Elem.from(docElem).filterDescendantElemsOrSelf(named(XsNamespace, "element"))
        .filter(_.attrOption("abstract").getOrElse("true").toBoolean)

    assertResult(indexedAbstractElemDecls.map(e => resolved.Elem.from(e))) {
      abstractElemDecls.map(e => resolved.Elem.from(e))
    }
  }

  test("testFindAncestorElem") {
    val docElem = axesSchemaDoc.documentElement

    assertResult(Set("http://www.nltaxonomie.nl/nt13/jenv/20181212/dictionary/jenv-bw2-axes")) {
      docElem.findAllDescendantElemsOrSelf().flatMap(_.findAncestorElem(_.attrOption("targetNamespace").nonEmpty))
        .map(_.attr("targetNamespace")).toSet
    }

    assertResult(Set(Some(docElem))) {
      docElem.findAllDescendantElems().map(_.findAncestorElem(_.attrOption("targetNamespace").nonEmpty)).toSet
    }
  }

  test("testFindPrecedingSiblingElems") {
    val docElem = axesSchemaDoc.documentElement

    val lastLinkbaseRef = docElem.filterDescendantElems(named(LinkNamespace, "linkbaseRef")).ensuring(_.nonEmpty).last

    val precedingLinkbaseRefs = lastLinkbaseRef.findAllPrecedingSiblingElems()

    assertResult(Seq.fill(4)(EName(LinkNamespace, "linkbaseRef"))) {
      precedingLinkbaseRefs.map(_.name)
    }

    assertResult(Seq("jenv-bw2-axes-lab-nl.xml", "jenv-bw2-axes-lab-fr.xml", "jenv-bw2-axes-lab-en.xml", "jenv-bw2-axes-lab-de.xml")) {
      precedingLinkbaseRefs.map(_.attr(XLinkNamespace, "href"))
    }

    assertResult(docElem.findDescendantElem(named(XsNamespace, "appinfo")).toList) {
      precedingLinkbaseRefs.flatMap(_.findParentElem()).distinct
    }

    assertResult(indexed.Elem.from(lastLinkbaseRef).findAllPrecedingSiblingElems().map(e => resolved.Elem.from(e))) {
      precedingLinkbaseRefs.map(e => resolved.Elem.from(e))
    }
  }

  private val axesSchemaXml =
    """
      |<!--
      |  This file is part of the Dutch Taxonomy (Nederlandse Taxonomie; NT)
      |  Intellectual Property of the State of the Netherlands
      |  Architecture: NT13
      |  Version: 20181212
      |  Release date: Tue Nov 6 09:00:00 2018
      |-->
      |<xs:schema xmlns:jenv-bw2-dim="http://www.nltaxonomie.nl/nt13/jenv/20181212/dictionary/jenv-bw2-axes" xmlns:link="http://www.xbrl.org/2003/linkbase" xmlns:xbrldt="http://xbrl.org/2005/xbrldt" xmlns:xbrli="http://www.xbrl.org/2003/instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xs="http://www.w3.org/2001/XMLSchema" attributeFormDefault="unqualified" elementFormDefault="qualified" targetNamespace="http://www.nltaxonomie.nl/nt13/jenv/20181212/dictionary/jenv-bw2-axes">
      |  <xs:annotation>
      |    <xs:appinfo>
      |      <link:linkbaseRef xlink:arcrole="http://www.w3.org/1999/xlink/properties/linkbase" xlink:href="jenv-bw2-axes-lab-de.xml" xlink:role="http://www.xbrl.org/2003/role/labelLinkbaseRef" xlink:type="simple"/>
      |      <link:linkbaseRef xlink:arcrole="http://www.w3.org/1999/xlink/properties/linkbase" xlink:href="jenv-bw2-axes-lab-en.xml" xlink:role="http://www.xbrl.org/2003/role/labelLinkbaseRef" xlink:type="simple"/>
      |      <link:linkbaseRef xlink:arcrole="http://www.w3.org/1999/xlink/properties/linkbase" xlink:href="jenv-bw2-axes-lab-fr.xml" xlink:role="http://www.xbrl.org/2003/role/labelLinkbaseRef" xlink:type="simple"/>
      |      <link:linkbaseRef xlink:arcrole="http://www.w3.org/1999/xlink/properties/linkbase" xlink:href="jenv-bw2-axes-lab-nl.xml" xlink:role="http://www.xbrl.org/2003/role/labelLinkbaseRef" xlink:type="simple"/>
      |      <link:linkbaseRef xlink:arcrole="http://www.w3.org/1999/xlink/properties/linkbase" xlink:href="jenv-bw2-axes-ref.xml" xlink:role="http://www.xbrl.org/2003/role/referenceLinkbaseRef" xlink:type="simple"/>
      |    </xs:appinfo>
      |  </xs:annotation>
      |  <xs:import namespace="http://www.nltaxonomie.nl/nt13/jenv/20181212/dictionary/jenv-bw2-domains" schemaLocation="../../20181212/dictionary/jenv-bw2-domains.xsd"/>
      |  <xs:import namespace="http://www.xbrl.org/2003/instance" schemaLocation="http://www.xbrl.org/2003/xbrl-instance-2003-12-31.xsd"/>
      |  <xs:import namespace="http://www.xbrl.org/2003/linkbase" schemaLocation="http://www.xbrl.org/2003/xbrl-linkbase-2003-12-31.xsd"/>
      |  <xs:import namespace="http://xbrl.org/2005/xbrldt" schemaLocation="http://www.xbrl.org/2005/xbrldt-2005.xsd"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_AverageNumberEmployeesSegmentsAxis" name="AverageNumberEmployeesSegmentsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_SegmentForAverageNumberEmployeesTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_BasisOfPreparationAxis" name="BasisOfPreparationAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfDirectorsAndPersonnelAxis" name="ClassesOfDirectorsAndPersonnelAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfDirectorsAxis" name="ClassesOfDirectorsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfDirectorsRemunerationAxis" name="ClassesOfDirectorsRemunerationAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfEquityAxis" name="ClassesOfEquityAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfFinancialAssetsAxis" name="ClassesOfFinancialAssetsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfFinancialInstrumentsMeasuredAtFairValueAxis" name="ClassesOfFinancialInstrumentsMeasuredAtFairValueAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfFinancialInstrumentsMeasuredAtHigherValueThanFairValueAxis" name="ClassesOfFinancialInstrumentsMeasuredAtHigherValueThanFairValueAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfIntangibleAssetsAxis" name="ClassesOfIntangibleAssetsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfLiabilitiesAxis" name="ClassesOfLiabilitiesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfLoansAdvancesGuaranteesAxis" name="ClassesOfLoansAdvancesGuaranteesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfPropertyPlantEquipmentAxis" name="ClassesOfPropertyPlantEquipmentAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ClassesOfShareCapitalAxis" name="ClassesOfShareCapitalAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_DerivativesAxis" name="DerivativesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_DerivativeFinancialInstrumentTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_EmployeeBenefitsExpensesOtherAxis" name="EmployeeBenefitsExpensesOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_EmployeeBenefitsExpensesOtherTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_FinancialAssetsOtherAxis" name="FinancialAssetsOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_FinancialAssetsOtherTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_FinancialInstrumentsCurrentValueAxis" name="FinancialInstrumentsCurrentValueAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_FinancialInstrumentsCurrentValueTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_FinancialInstrumentsHigherValueThanFairValueAxis" name="FinancialInstrumentsHigherValueThanFairValueAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_FinancialInstrumentsHigherValueThanFairValueTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_FinancialStatementsTypeAxis" name="FinancialStatementsTypeAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_HedgeAccountingAxis" name="HedgeAccountingAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_HedgeInstrumentTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_IncomeTaxExpenseOtherAxis" name="IncomeTaxExpenseOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_IncomeTaxExpenseOtherTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_IntangibleAssetsOtherAxis" name="IntangibleAssetsOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_IntangibleAssetsOtherTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_LegalEntityNamesConsolidatedInterestsAxis" name="LegalEntityNamesConsolidatedInterestsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_LegalEntityTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_LegalEntityNamesGroupStructureAxis" name="LegalEntityNamesGroupStructureAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_LegalEntityTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_LegalEntityNamesParticipatingInterestsAxis" name="LegalEntityNamesParticipatingInterestsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_LegalEntityTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_LoansAdvancesGuaranteesAxis" name="LoansAdvancesGuaranteesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ManagingOrSupervisoryDirectorOrRepresentativeTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ManagingDirectorNamesLoansAdvancesGuaranteesAxis" name="ManagingDirectorNamesLoansAdvancesGuaranteesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ManagingDirectorTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ManagingDirectorNamesResultsAppropriationAxis" name="ManagingDirectorNamesResultsAppropriationAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ManagingDirectorTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ManagingDirectorNamesRightsGrantedNotYetExercisedAxis" name="ManagingDirectorNamesRightsGrantedNotYetExercisedAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ManagingDirectorTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ManagingDirectorNamesRightsGrantedNotYetExercisedMovementAxis" name="ManagingDirectorNamesRightsGrantedNotYetExercisedMovementAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ManagingDirectorTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ManagingOrSupervisoryDirectorOrRepresentativeNamesAxis" name="ManagingOrSupervisoryDirectorOrRepresentativeNamesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ManagingOrSupervisoryDirectorOrRepresentativeTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ManagmentBoardCompositionNamesAxis" name="ManagmentBoardCompositionNamesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ManagingOrSupervisoryDirectorOrRepresentativeTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_NetRevenueGeographicSegmentsAxis" name="NetRevenueGeographicSegmentsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_NetRevenueGeographicTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_NetRevenueIndustrySegmentsAxis" name="NetRevenueIndustrySegmentsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_NetRevenueIndustryTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_OperatingExpensesOtherAxis" name="OperatingExpensesOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_OperatingExpensesOtherTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_OperatingIncomeOtherAxis" name="OperatingIncomeOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_OperatingIncomeOtherTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_PayablesOtherCurrentAxis" name="PayablesOtherCurrentAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_PayablesOtherCurrentTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_PayablesOtherNoncurrentAxis" name="PayablesOtherNoncurrentAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_PayablesOtherNoncurrentTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ProvisionsOtherAxis" name="ProvisionsOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ProvisionsOtherTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ReceivablesOtherCurrentAxis" name="ReceivablesOtherCurrentAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ReceivablesOtherCurrentTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_RelatedPartyTransactionsDetailsAxis" name="RelatedPartyTransactionsDetailsAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_RelatedPartyTransactionDetailsTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_RevenuesOtherAxis" name="RevenuesOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_RevenuesOtherTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ShareBasedPaymentsSpecificationAxis" name="ShareBasedPaymentsSpecificationAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ShareBasedPaymentsSpecificationTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ShareCapitalAxis" name="ShareCapitalAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ShareCapitalNumberOfSharesTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ShareCapitalChangesAxis" name="ShareCapitalChangesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ShareCapitalTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ShareCapitalDisclosureAxis" name="ShareCapitalDisclosureAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ShareCapitalTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_ShareCapitalEarningsPerShareAxis" name="ShareCapitalEarningsPerShareAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_ShareCapitalEarningsPerShareTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_SpecialGoverningControlStructureAxis" name="SpecialGoverningControlStructureAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_NameBeneficiarySpecialGoverningControlStructureTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_SupervisoryDirectorNamesAxis" name="SupervisoryDirectorNamesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_SupervisoryDirectorTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_SupervisoryDirectorNamesDirectorLoansAdvancesGuaranteesAxis" name="SupervisoryDirectorNamesDirectorLoansAdvancesGuaranteesAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_SupervisoryDirectorTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_SupervisoryDirectorNamesRightsGrantedNotYetExercisedAxis" name="SupervisoryDirectorNamesRightsGrantedNotYetExercisedAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_SupervisoryDirectorTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_SupervisoryDirectorNamesRightsGrantedNotYetExercisedMovementAxis" name="SupervisoryDirectorNamesRightsGrantedNotYetExercisedMovementAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_SupervisoryDirectorTypedMember" xbrli:periodType="duration"/>
      |  <xs:element abstract="true" id="jenv-bw2-dim_TangibleAssetsOtherAxis" name="TangibleAssetsOtherAxis" nillable="false" substitutionGroup="xbrldt:dimensionItem" type="xbrli:stringItemType" xbrldt:typedDomainRef="jenv-bw2-domains.xsd#jenv-bw2-dm_TangibleAssetsOtherTypedMember" xbrli:periodType="duration"/>
      |</xs:schema>
      |
    """.stripMargin.trim

  private val axesSchemaDoc: JsDomDocument = {
    val domParser = new DOMParser()
    JsDomDocument(domParser.parseFromString(axesSchemaXml, SupportedType.`text/xml`))
  }

  private val XsNamespace = "http://www.w3.org/2001/XMLSchema"
  private val LinkNamespace = "http://www.xbrl.org/2003/linkbase"
  private val XLinkNamespace = "http://www.w3.org/1999/xlink"
}
