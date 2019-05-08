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

import java.io.File
import java.net.URI
import java.time.LocalDate

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.dialect.AbstractDialectClarkElem
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkDocumentApi
import eu.cdevreeze.yaidom2.queryapi.oo.named
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.oo.DocumentApi
import eu.cdevreeze.yaidom2.queryapi.oo.elemstep.ClarkElemStepFactory
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite

/**
 * Query test using a "yaidom dialect" (for so-called XBRL Taxonomy Packages).
 *
 * This test shows how "yaidom dialects" and underlying element implementations are decoupled and can be combined
 * at runtime in any way that is desired.
 *
 * @author Chris de Vreeze
 */
abstract class TpDialectOverClarkElemQueryTest extends AnyFunSuite {

  import TpDialectOverClarkElemQueryTest._

  private val processor = new Processor(false)

  protected def document: ClarkDocumentApi

  private def rootElem: ClarkNodes.Elem = document.documentElement

  protected def saxonDocument: saxon.Document = {
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[TpDialectOverClarkElemQueryTest].getResource("/test-xml/taxonomyPackage.xml").toURI)
    val doc = docBuilder.build(file)

    saxon.Document(doc)
  }

  protected def saxonRootElem: saxon.Elem = {
    saxonDocument.documentElement
  }

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

object TpDialectOverClarkElemQueryTest {

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

  final case class TpDocument(underlyingDoc: ClarkDocumentApi) extends DocumentApi {
    require(underlyingDoc.documentElement.name == TpTaxonomyPackageEName, s"Expected TaxonomyPackage document element")

    type NodeType = TpNode

    type CanBeDocumentChildType = TpCanBeDocumentChild

    type ElemType = TpElem

    def docUriOption: Option[URI] = underlyingDoc.docUriOption

    def documentElement: TaxonomyPackage = TaxonomyPackage(underlyingDoc.documentElement)

    def children: Seq[TpCanBeDocumentChild] = ArraySeq(documentElement)
  }

  /**
   * Arbitrary TP node
   */
  sealed trait TpNode extends ClarkNodes.Node

  /**
   * Potential document child
   */
  sealed trait TpCanBeDocumentChild extends TpNode with ClarkNodes.CanBeDocumentChild

  /**
   * TP dialect element node, offering the `ClarkNodes.Elem` element query API.
   *
   * Note that this TP element can work with any underlying `ClarkNodes.Elem` element,
   * using its "raw" type, thus making the API easy to use.
   */
  sealed abstract class TpElem(
    underlyingElem: ClarkNodes.Elem
  ) extends AbstractDialectClarkElem(underlyingElem) with TpCanBeDocumentChild {

    type ThisElem = TpElem

    type ThisNode = TpNode

    final def wrapElem(underlyingElem: ClarkNodes.Elem): ThisElem = TpElem(underlyingElem)

    // ClarkNodes.Elem

    final def children: ArraySeq[TpNode] = {
      underlyingElem.children.flatMap(TpNode.opt).to(ArraySeq)
    }

    final def select(step: ElemStep[TpElem]): Seq[TpElem] = {
      step(this)
    }
  }

  final case class TaxonomyPackage(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def findAllEntryPoints(): Seq[EntryPoint] = {
      filterDescendantElems(havingType[EntryPoint]).collect { case e: EntryPoint => e }
    }

    def filterEntryPoints(p: EntryPoint => Boolean): Seq[EntryPoint] = {
      filterDescendantElems(where[EntryPoint](p)).collect { case e: EntryPoint => e }
    }

    def findEntryPoint(p: EntryPoint => Boolean): Option[EntryPoint] = {
      findDescendantElem(where[EntryPoint](p)).collectFirst { case e: EntryPoint => e }
    }

    def getEntryPoint(p: EntryPoint => Boolean): EntryPoint = {
      findDescendantElem(where[EntryPoint](p)).collectFirst { case e: EntryPoint => e }
        .getOrElse(sys.error(s"Missing entryPoint obeying the given predicate"))
    }

    // Child elements

    def getIdentifier: Identifier = {
      findChildElem(havingType[Identifier]).collectFirst { case e: Identifier => e }.get
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
      findChildElem(havingType[Version]).collectFirst { case e: Version => e }
    }

    def findLicense: Option[License] = {
      findChildElem(havingType[License]).collectFirst { case e: License => e }
    }

    def findAllPublishers: Seq[Publisher] = {
      filterChildElems(havingType[Publisher]).collect { case e: Publisher => e }
    }

    def findPublisherUrl: Option[PublisherUrl] = {
      findChildElem(havingType[PublisherUrl]).collectFirst { case e: PublisherUrl => e }
    }

    def findPublisherCountry: Option[PublisherCountry] = {
      findChildElem(havingType[PublisherCountry]).collectFirst { case e: PublisherCountry => e }
    }

    def findPublicationDate: Option[PublicationDate] = {
      findChildElem(havingType[PublicationDate]).collectFirst { case e: PublicationDate => e }
    }

    def findEntryPointsElem: Option[EntryPointsElem] = {
      findChildElem(havingType[EntryPointsElem]).collectFirst { case e: EntryPointsElem => e }
    }

    def findSupersededTaxonomyPackagesElem: Option[SupersededTaxonomyPackagesElem] = {
      findChildElem(havingType[SupersededTaxonomyPackagesElem]).collectFirst { case e: SupersededTaxonomyPackagesElem => e }
    }

    def findVersioningReportsElem: Option[VersioningReportsElem] = {
      findChildElem(havingType[VersioningReportsElem]).collectFirst { case e: VersioningReportsElem => e }
    }
  }

  final case class Identifier(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def value: URI = URI.create(text)
  }

  sealed trait DocumentationGroup extends TpElem {

    def value: String
  }

  final case class Name(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) with DocumentationGroup {

    def value: String = text
  }

  final case class Description(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) with DocumentationGroup {

    def value: String = text
  }

  final case class Version(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def value: String = text
  }

  final case class License(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def href: URI = URI.create(attr(HrefEName))

    def licenseName: String = attr(NameEName)

    def value: String = text
  }

  final case class Publisher(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def value: String = text
  }

  final case class PublisherUrl(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def value: URI = URI.create(text)
  }

  final case class PublisherCountry(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def value: String = text
  }

  final case class PublicationDate(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    // Ignoring time zones, because dates without times are unlikely to contain time zones.
    def value: LocalDate = LocalDate.parse(text)
  }

  final case class EntryPointsElem(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def findAllEntryPoints: Seq[EntryPoint] = {
      filterChildElems(havingType[EntryPoint]).collect { case e: EntryPoint => e }
    }
  }

  final case class SupersededTaxonomyPackagesElem(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def findAllTaxonomyPackageRefs: Seq[TaxonomyPackageRef] = {
      filterChildElems(havingType[TaxonomyPackageRef]).collect { case e: TaxonomyPackageRef => e }
    }
  }

  final case class VersioningReportsElem(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def findAllVersioningReports: Seq[VersioningReport] = {
      filterChildElems(havingType[VersioningReport]).collect { case e: VersioningReport => e }
    }
  }

  final case class EntryPoint(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

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
      findChildElem(havingType[Version]).collectFirst { case e: Version => e }
    }

    def findAllEntryPointDocuments: Seq[EntryPointDocument] = {
      filterChildElems(havingType[EntryPointDocument]).collect { case e: EntryPointDocument => e }
    }

    def findLanguagesElem: Option[LanguagesElem] = {
      findChildElem(havingType[LanguagesElem]).collectFirst { case e: LanguagesElem => e }
    }
  }

  final case class EntryPointDocument(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def href: URI = URI.create(attr(HrefEName))
  }

  final case class VersioningReport(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def href: URI = URI.create(attr(HrefEName))
  }

  final case class LanguagesElem(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def findAllLanguages: Seq[Language] = filterChildElems(havingType[Language]).collect { case e: Language => e }
  }

  final case class Language(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def value: String = text
  }

  final case class TaxonomyPackageRef(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem) {

    def value: URI = URI.create(text)
  }

  final case class OtherTpElem(override val underlyingElem: ClarkNodes.Elem) extends TpElem(underlyingElem)

  /**
   * TP text node
   */
  final case class TpText(text: String) extends TpNode with ClarkNodes.Text

  object TpNode {

    def opt(underlyingNode: ClarkNodes.Node): Option[TpNode] = {
      underlyingNode match {
        case e: ClarkNodes.Elem => Some(TpElem(e))
        case t: ClarkNodes.Text => Some(TpText(t.text))
        case _ => None
      }
    }
  }

  object TpElem {

    def apply(underlyingElem: ClarkNodes.Elem): TpElem = {
      constructors.get(underlyingElem.name).map(f => f(underlyingElem)).getOrElse(OtherTpElem(underlyingElem))
    }

    // Fast construction using Map taking element name as key

    private val constructors: Map[EName, ClarkNodes.Elem => TpElem] = Map(
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

    def apply(underlyingElem: ClarkNodes.Elem): TaxonomyPackage = {
      require(underlyingElem.name == TpTaxonomyPackageEName, s"Expected taxonomy package but got '${underlyingElem.name}'")
      new TaxonomyPackage(underlyingElem)
    }
  }

  /**
   * ElemStep factory API for TP elements.
   */
  object TpElemSteps extends ClarkElemStepFactory {

    type ElemType = TpElem
  }

}
