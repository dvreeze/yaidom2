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

import java.net.URI
import java.time.LocalDate

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.dialect.AbstractDialectScopedElem
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.DocumentApi
import eu.cdevreeze.yaidom2.queryapi.ScopedDocumentApi
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.elemstep.ScopedElemStepFactory
import eu.cdevreeze.yaidom2.queryapi.named
import org.scalatest.FunSuite

/**
 * Query test using a "yaidom dialect" (for so-called XBRL Taxonomy Packages).
 *
 * This test shows how "yaidom dialects" and underlying element implementations are decoupled and can be combined
 * at runtime in any way that is desired.
 *
 * @author Chris de Vreeze
 */
trait TpDialectOverScopedElemQueryTest extends FunSuite {

  import TpDialectOverScopedElemQueryTest._

  protected def document: ScopedDocumentApi

  private def rootElem: ScopedNodes.Elem = document.documentElement

  test("testQueryEntrypoints") {
    val taxoPackage = TaxonomyPackage(rootElem)

    assertResult(rootElem.filterDescendantElems(named(TpEntryPointEName))
      .flatMap(_.filterChildElems(named(TpNameEName))).map(_.text).toSet) {

      taxoPackage.findAllEntryPoints().flatMap(_.findAllNames).map(_.value).toSet
    }
  }

  test("testEquivalenceOfEntrypointQueries") {
    val taxoPackage = TaxonomyPackage(rootElem)

    assertResult(rootElem.filterDescendantElems(named(TpEntryPointEName))
      .map(e => resolved.Elem.from(e))) {

      taxoPackage.findAllEntryPoints().map(e => resolved.Elem.from(e))
    }
  }

  test("testQueryOneEntrypoint") {
    val taxoPackage = TaxonomyPackage(rootElem)

    val entrypointName = "VPB rapportage 2017"

    val entrypointOption = taxoPackage.findEntryPoint(_.findAllNames.exists(_.value == entrypointName))

    assertResult(true) {
      entrypointOption.nonEmpty
    }

    val entrypoint = entrypointOption.get

    assertResult(Seq(entrypointName)) {
      entrypoint.findAllNames.map(_.value)
    }
    assertResult(Seq("De vennootschapsbelastingsrapportage 2017 ter deponering bij de banken.")) {
      entrypoint.findAllDescriptions.map(_.value)
    }
    assertResult(Some("20171201")) {
      entrypoint.findVersion.map(_.value)
    }
    assertResult(Seq("https://www.sbrbanken.nl/bt12/frc/20171201/entrypoints/frc-rpt-vpb-2017.xsd")) {
      entrypoint.findAllEntryPointDocuments.map(_.href.toString)
    }
  }

  test("testQueryEntrypointDocuments") {
    val taxoPackage = TaxonomyPackage(rootElem)

    val docs = taxoPackage.findAllEntryPoints().flatMap(_.findAllEntryPointDocuments)

    assertResult(true) {
      docs.size > 10
    }
    assertResult(taxoPackage.filterDescendantElems(named(TpEntryPointDocumentEName))) {
      docs
    }
  }

  test("testResolvedElemProperty") {
    val taxonomyPackage = TaxonomyPackage(rootElem)

    assertResult(resolved.Elem.from(taxonomyPackage).findAllDescendantElemsOrSelf()) {
      taxonomyPackage.findAllDescendantElemsOrSelf().map(e => resolved.Elem.from(e))
    }
  }

  test("testResolvedElemPropertyViaDocument") {
    val taxonomyPackageDoc = TpDocument(document)

    assertResult(resolved.Elem.from(taxonomyPackageDoc.documentElement).findAllDescendantElemsOrSelf()) {
      taxonomyPackageDoc.documentElement.findAllDescendantElemsOrSelf().map(e => resolved.Elem.from(e))
    }
  }
}

object TpDialectOverScopedElemQueryTest {

  // First the general yaidom dialect support

  def havingType[T: ClassTag]: ClarkNodes.Elem => Boolean = {
    {
      case e: T => true
      case e => false
    }
  }

  def where[T: ClassTag](p: T => Boolean): ClarkNodes.Elem => Boolean = {
    {
      case e: T if p(e) => true
      case e => false
    }
  }

  // Below is the TP (Taxonomy Package) specific dialect support

  val TpNs = "http://xbrl.org/2016/taxonomy-package"

  val TpTaxonomyPackageEName = EName(TpNs, "taxonomyPackage")
  val TpIdentifierEName = EName(TpNs, "identifier")
  val TpVersionEName = EName(TpNs, "version")
  val TpLicenseEName = EName(TpNs, "license")
  val TpPublisherEName = EName(TpNs, "publisher")
  val TpPublisherURLEName = EName(TpNs, "publisherURL")
  val TpPublisherCountryEName = EName(TpNs, "publisherCountry")
  val TpPublicationDateEName = EName(TpNs, "publicationDate")
  val TpEntryPointsEName = EName(TpNs, "entryPoints")
  val TpEntryPointEName = EName(TpNs, "entryPoint")
  val TpSupersededTaxonomyPackagesEName = EName(TpNs, "supersededTaxonomyPackages")
  val TpVersioningReportsEName = EName(TpNs, "versioningReports")
  val TpEntryPointDocumentEName = EName(TpNs, "entryPointDocument")
  val TpLanguagesEName = EName(TpNs, "languages")
  val TpTaxonomyPackageRefEName = EName(TpNs, "taxonomyPackageRef")
  val TpVersioningReportEName = EName(TpNs, "versioningReport")
  val TpNameEName = EName(TpNs, "name")
  val TpDescriptionEName = EName(TpNs, "description")
  val TpLanguageEName = EName(TpNs, "language")

  val HrefEName = EName.fromLocalName("href")
  val NameEName = EName.fromLocalName("name")

  final case class TpDocument(underlyingDoc: ScopedDocumentApi) extends DocumentApi {
    require(underlyingDoc.documentElement.name == TpTaxonomyPackageEName, s"Expected TaxonomyPackage document element")

    type NodeType = TpNode

    type CanBeDocumentChildType = TpCanBeDocumentChild

    type ElemType = TpElem

    def docUriOption: Option[URI] = underlyingDoc.docUriOption

    def documentElement: TaxonomyPackage = TaxonomyPackage(underlyingDoc.documentElement)

    def children: Seq[TpCanBeDocumentChild] = {
      underlyingDoc.children.map {
        case e: ScopedNodes.Elem => documentElement
        case c: ScopedNodes.Comment => TpComment(c.text)
        case pi: ScopedNodes.ProcessingInstruction => TpProcessingInstruction(pi.target, pi.data)
      }
    }
  }

  /**
   * Arbitrary TP node
   */
  sealed trait TpNode extends ScopedNodes.Node

  /**
   * Potential document child
   */
  sealed trait TpCanBeDocumentChild extends TpNode with ScopedNodes.CanBeDocumentChild

  /**
   * TP dialect element node, offering the `ScopedNodes.Elem` element query API.
   *
   * Note that this TP element can work with any underlying `ScopedNodes.Elem` element,
   * using its "raw" type, thus making the API easy to use.
   */
  sealed trait TpElem extends AbstractDialectScopedElem with TpCanBeDocumentChild {

    type ThisElem = TpElem

    type ThisNode = TpNode

    def underlyingElem: ScopedNodes.Elem

    final def wrapElem(underlyingElem: ScopedNodes.Elem): ThisElem = TpElem(underlyingElem)

    // ClarkNodes.Elem

    final def children: ArraySeq[TpNode] = {
      underlyingElem.children.flatMap(TpNode.opt).to(ArraySeq)
    }

    final def select(step: ElemStep[TpElem]): Seq[TpElem] = {
      step(this)
    }
  }

  final case class TaxonomyPackage(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def findAllEntryPoints(): Seq[EntryPoint] = {
      filterDescendantElems(havingType[EntryPoint]).collect { case e: EntryPoint => e }
    }

    def filterEntryPoints(p: EntryPoint => Boolean): Seq[EntryPoint] = {
      filterDescendantElems(where[EntryPoint](p)).collect { case e: EntryPoint => e }
    }

    def findEntryPoint(p: EntryPoint => Boolean): Option[EntryPoint] = {
      findDescendantElem(where[EntryPoint](p)).toList.collectFirst { case e: EntryPoint => e }
    }

    def getEntryPoint(p: EntryPoint => Boolean): EntryPoint = {
      findDescendantElem(where[EntryPoint](p)).toList.collectFirst { case e: EntryPoint => e }
        .getOrElse(sys.error(s"Missing entryPoint obeying the given predicate"))
    }

    // Child elements

    def getIdentifier: Identifier = {
      findChildElem(havingType[Identifier]).toList.collectFirst { case e: Identifier => e }.get
    }

    def findAllDocumentationGroups: Seq[DocumentationGroup] = {
      filterChildElems(havingType[DocumentationGroup]).collect { case e: DocumentationGroup => e }
    }

    def findAllNames: Seq[Name] = {
      filterChildElems(havingType[Name]).collect { case e: Name => e }
    }

    def findAllDescriptions: Seq[Description] = {
      filterChildElems(havingType[Description]).collect { case e: Description => e }
    }

    def findVersion: Option[Version] = {
      findChildElem(havingType[Version]).toList.collectFirst { case e: Version => e }
    }

    def findLicense: Option[License] = {
      findChildElem(havingType[License]).toList.collectFirst { case e: License => e }
    }

    def findAllPublishers: Seq[Publisher] = {
      filterChildElems(havingType[Publisher]).collect { case e: Publisher => e }
    }

    def findPublisherUrl: Option[PublisherUrl] = {
      findChildElem(havingType[PublisherUrl]).toList.collectFirst { case e: PublisherUrl => e }
    }

    def findPublisherCountry: Option[PublisherCountry] = {
      findChildElem(havingType[PublisherCountry]).toList.collectFirst { case e: PublisherCountry => e }
    }

    def findPublicationDate: Option[PublicationDate] = {
      findChildElem(havingType[PublicationDate]).toList.collectFirst { case e: PublicationDate => e }
    }

    def findEntryPointsElem: Option[EntryPointsElem] = {
      findChildElem(havingType[EntryPointsElem]).toList.collectFirst { case e: EntryPointsElem => e }
    }

    def findSupersededTaxonomyPackagesElem: Option[SupersededTaxonomyPackagesElem] = {
      findChildElem(havingType[SupersededTaxonomyPackagesElem]).toList.collectFirst { case e: SupersededTaxonomyPackagesElem => e }
    }

    def findVersioningReportsElem: Option[VersioningReportsElem] = {
      findChildElem(havingType[VersioningReportsElem]).toList.collectFirst { case e: VersioningReportsElem => e }
    }
  }

  final case class Identifier(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  sealed trait DocumentationGroup extends TpElem {

    def value: String
  }

  final case class Name(underlyingElem: ScopedNodes.Elem) extends TpElem with DocumentationGroup {

    def value: String = text
  }

  final case class Description(underlyingElem: ScopedNodes.Elem) extends TpElem with DocumentationGroup {

    def value: String = text
  }

  final case class Version(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class License(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))

    def licenseName: String = attr(NameEName)

    def value: String = text
  }

  final case class Publisher(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class PublisherUrl(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  final case class PublisherCountry(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class PublicationDate(underlyingElem: ScopedNodes.Elem) extends TpElem {

    // Ignoring time zones, because dates without times are unlikely to contain time zones.
    def value: LocalDate = LocalDate.parse(text)
  }

  final case class EntryPointsElem(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def findAllEntryPoints: Seq[EntryPoint] = {
      filterChildElems(havingType[EntryPoint]).collect { case e: EntryPoint => e }
    }
  }

  final case class SupersededTaxonomyPackagesElem(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def findAllTaxonomyPackageRefs: Seq[TaxonomyPackageRef] = {
      filterChildElems(havingType[TaxonomyPackageRef]).collect { case e: TaxonomyPackageRef => e }
    }
  }

  final case class VersioningReportsElem(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def findAllVersioningReports: Seq[VersioningReport] = {
      filterChildElems(havingType[VersioningReport]).collect { case e: VersioningReport => e }
    }
  }

  final case class EntryPoint(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def findAllEntryPointHrefs: Seq[URI] = {
      findAllEntryPointDocuments.map(_.href)
    }

    def findAllDocumentationGroups: Seq[DocumentationGroup] = {
      filterChildElems(havingType[DocumentationGroup]).collect { case e: DocumentationGroup => e }
    }

    def findAllNames: Seq[Name] = {
      filterChildElems(havingType[Name]).collect { case e: Name => e }
    }

    def findAllDescriptions: Seq[Description] = {
      filterChildElems(havingType[Description]).collect { case e: Description => e }
    }

    def findVersion: Option[Version] = {
      findChildElem(havingType[Version]).toList.collectFirst { case e: Version => e }
    }

    def findAllEntryPointDocuments: Seq[EntryPointDocument] = {
      filterChildElems(havingType[EntryPointDocument]).collect { case e: EntryPointDocument => e }
    }

    def findLanguagesElem: Option[LanguagesElem] = {
      findChildElem(havingType[LanguagesElem]).toList.collectFirst { case e: LanguagesElem => e }
    }
  }

  final case class EntryPointDocument(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))
  }

  final case class VersioningReport(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))
  }

  final case class LanguagesElem(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def findAllLanguages: Seq[Language] = filterChildElems(havingType[Language]).collect { case e: Language => e }
  }

  final case class Language(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class TaxonomyPackageRef(underlyingElem: ScopedNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  final case class OtherTpElem(underlyingElem: ScopedNodes.Elem) extends TpElem

  /**
   * TP text node
   */
  final case class TpText(text: String) extends TpNode with ScopedNodes.Text

  /**
   * TP comment node
   */
  final case class TpComment(text: String) extends TpCanBeDocumentChild with ScopedNodes.Comment

  /**
   * TP processing instruction node
   */
  final case class TpProcessingInstruction(target: String, data: String) extends TpCanBeDocumentChild with ScopedNodes.ProcessingInstruction

  object TpNode {

    def opt(underlyingNode: ScopedNodes.Node): Option[TpNode] = {
      underlyingNode match {
        case e: ScopedNodes.Elem => Some(TpElem(e))
        case t: ScopedNodes.Text => Some(TpText(t.text))
        case c: ScopedNodes.Comment => Some(TpComment(c.text))
        case pi: ScopedNodes.ProcessingInstruction => Some(TpProcessingInstruction(pi.target, pi.data))
        case _ => None
      }
    }
  }

  object TpElem {

    def apply(underlyingElem: ScopedNodes.Elem): TpElem = {
      constructors.get(underlyingElem.name).map(f => f(underlyingElem)).getOrElse(OtherTpElem(underlyingElem))
    }

    // Fast construction using Map taking element name as key

    private val constructors: Map[EName, ScopedNodes.Elem => TpElem] = Map(
      TpTaxonomyPackageEName -> { e => new TaxonomyPackage(e) },
      TpIdentifierEName -> { e => new Identifier(e) },
      TpVersionEName -> { e => new Version(e) },
      TpLicenseEName -> { e => new License(e) },
      TpPublisherEName -> { e => new Publisher(e) },
      TpPublisherURLEName -> { e => new PublisherUrl(e) },
      TpPublisherCountryEName -> { e => new PublisherCountry(e) },
      TpPublicationDateEName -> { e => new PublicationDate(e) },
      TpEntryPointsEName -> { e => new EntryPointsElem(e) },
      TpEntryPointEName -> { e => new EntryPoint(e) },
      TpSupersededTaxonomyPackagesEName -> { e => new SupersededTaxonomyPackagesElem(e) },
      TpVersioningReportsEName -> { e => new VersioningReportsElem(e) },
      TpEntryPointDocumentEName -> { e => new EntryPointDocument(e) },
      TpLanguagesEName -> { e => new LanguagesElem(e) },
      TpTaxonomyPackageRefEName -> { e => new TaxonomyPackageRef(e) },
      TpVersioningReportEName -> { e => new VersioningReport(e) },
      TpNameEName -> { e => new Name(e) },
      TpDescriptionEName -> { e => new Description(e) },
      TpLanguageEName -> { e => new Language(e) },
    )
  }

  object TaxonomyPackage {

    def apply(underlyingElem: ScopedNodes.Elem): TaxonomyPackage = {
      require(underlyingElem.name == TpTaxonomyPackageEName, s"Expected taxonomy package but got '${underlyingElem.name}'")
      new TaxonomyPackage(underlyingElem)
    }
  }

  /**
   * ElemStep factory API for TP elements.
   */
  object TpElemSteps extends ScopedElemStepFactory {

    type ElemType = TpElem
  }

}
