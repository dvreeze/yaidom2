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
import eu.cdevreeze.yaidom2.dialect.AbstractDialectBackingElem
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.BackingDocumentApi
import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.DocumentApi
import eu.cdevreeze.yaidom2.queryapi.elemstep.BackingElemStepFactory
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
trait TpDialectOverBackingElemQueryTest extends FunSuite {

  import TpDialectOverBackingElemQueryTest._

  protected def document: BackingDocumentApi

  private def rootElem: BackingNodes.Elem = document.documentElement

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

  test("testQueryEntrypointOfEntrypointDocuments") {
    val taxoPackage = TaxonomyPackage(rootElem)

    val docs = taxoPackage.findAllEntryPoints().flatMap(_.findAllEntryPointDocuments)

    assertResult(true) {
      docs.size > 10
    }
    assertResult(taxoPackage.filterDescendantElems(named(TpEntryPointDocumentEName))) {
      docs
    }

    assertResult(taxoPackage.findAllEntryPoints()) {
      docs.map(_.entryPoint).distinct
    }
  }

  test("testQueryRootOfAllDescendantElemsOrSelf") {
    val taxoPackage = TaxonomyPackage(rootElem)

    assertResult(Seq(taxoPackage)) {
      taxoPackage.findAllDescendantElemsOrSelf().map(_.taxonomyPackage).distinct
    }
  }

  test("testResolvedElemPropertyViaDocument") {
    val taxonomyPackageDoc = TpDocument(document)

    assertResult(resolved.Elem.from(taxonomyPackageDoc.documentElement).findAllDescendantElemsOrSelf()) {
      taxonomyPackageDoc.documentElement.findAllDescendantElemsOrSelf().map(e => resolved.Elem.from(e))
    }
  }
}

object TpDialectOverBackingElemQueryTest {

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

  final case class TpDocument(underlyingDoc: BackingDocumentApi) extends DocumentApi {
    require(underlyingDoc.documentElement.name == TpTaxonomyPackageEName, s"Expected TaxonomyPackage document element")

    type NodeType = TpNode

    type CanBeDocumentChildType = TpCanBeDocumentChild

    type ElemType = TpElem

    def docUriOption: Option[URI] = documentElement.docUriOption

    def documentElement: TaxonomyPackage = TaxonomyPackage(underlyingDoc.documentElement)

    def children: Seq[TpCanBeDocumentChild] = {
      underlyingDoc.children.map {
        case e: BackingNodes.Elem => documentElement
        case c: BackingNodes.Comment => TpComment(c.text)
        case pi: BackingNodes.ProcessingInstruction => TpProcessingInstruction(pi.target, pi.data)
      }
    }
  }

  /**
   * Arbitrary TP node
   */
  sealed trait TpNode extends BackingNodes.Node

  /**
   * Potential document child
   */
  sealed trait TpCanBeDocumentChild extends TpNode with BackingNodes.CanBeDocumentChild

  /**
   * TP dialect element node, offering the `BackingNodes.Elem` element query API.
   *
   * Note that this TP element can work with any underlying `BackingNodes.Elem` element,
   * using its "raw" type, thus making the API easy to use.
   */
  sealed trait TpElem extends AbstractDialectBackingElem with TpCanBeDocumentChild {

    type ThisElem = TpElem

    type ThisNode = TpNode

    final def wrapElem(underlyingElem: BackingNodes.Elem): ThisElem = TpElem(underlyingElem)

    // ClarkNodes.Elem

    final def children: ArraySeq[TpNode] = {
      underlyingElem.children.flatMap(TpNode.opt).to(ArraySeq)
    }

    final override def select(step: ElemStep[TpElem]): Seq[TpElem] = {
      step(this)
    }

    // Root taxonomy package

    final def taxonomyPackage: TaxonomyPackage = {
      findAncestorElemOrSelf(havingType[TaxonomyPackage]).collect { case e: TaxonomyPackage => e }.get
    }

    // Overriding methods that have type member ThisElem in the method signature, to "correct" the method signature now that ThisElem is known

    override def findAllChildElems(): Seq[ThisElem] = super.findAllChildElems()

    override def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = super.filterDescendantElems(p)

    override def findAllDescendantElems(): Seq[ThisElem] = super.findAllDescendantElems()

    override def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = super.findDescendantElem(p)

    override def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.filterDescendantElemsOrSelf(p)

    override def findAllDescendantElemsOrSelf(): Seq[ThisElem] = super.findAllDescendantElemsOrSelf()

    override def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = super.findDescendantElemOrSelf(p)

    override def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = super.findTopmostElems(p)

    override def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.findTopmostElemsOrSelf(p)

    override def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = super.getDescendantElemOrSelf(navigationPath)

    override def findParentElem(): Option[ThisElem] = super.findParentElem()

    override def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = super.filterAncestorElems(p)

    override def findAllAncestorElems(): Seq[ThisElem] = super.findAllAncestorElems()

    override def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = super.findAncestorElem(p)

    override def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.filterAncestorElemsOrSelf(p)

    override def findAllAncestorElemsOrSelf(): Seq[ThisElem] = super.findAllAncestorElemsOrSelf()

    override def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = super.findAncestorElemOrSelf(p)

    override def findAllPrecedingSiblingElems(): Seq[ThisElem] = super.findAllPrecedingSiblingElems()

    override def ownNavigationPathRelativeToRootElem: Seq[Int] = super.ownNavigationPathRelativeToRootElem
  }

  final case class TaxonomyPackage(underlyingElem: BackingNodes.Elem) extends TpElem {

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

  final case class Identifier(underlyingElem: BackingNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  sealed trait DocumentationGroup extends TpElem {

    def value: String
  }

  final case class Name(underlyingElem: BackingNodes.Elem) extends TpElem with DocumentationGroup {

    def value: String = text
  }

  final case class Description(underlyingElem: BackingNodes.Elem) extends TpElem with DocumentationGroup {

    def value: String = text
  }

  final case class Version(underlyingElem: BackingNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class License(underlyingElem: BackingNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))

    def licenseName: String = attr(NameEName)

    def value: String = text
  }

  final case class Publisher(underlyingElem: BackingNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class PublisherUrl(underlyingElem: BackingNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  final case class PublisherCountry(underlyingElem: BackingNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class PublicationDate(underlyingElem: BackingNodes.Elem) extends TpElem {

    // Ignoring time zones, because dates without times are unlikely to contain time zones.
    def value: LocalDate = LocalDate.parse(text)
  }

  final case class EntryPointsElem(underlyingElem: BackingNodes.Elem) extends TpElem {

    def findAllEntryPoints: Seq[EntryPoint] = {
      filterChildElems(havingType[EntryPoint]).collect { case e: EntryPoint => e }
    }
  }

  final case class SupersededTaxonomyPackagesElem(underlyingElem: BackingNodes.Elem) extends TpElem {

    def findAllTaxonomyPackageRefs: Seq[TaxonomyPackageRef] = {
      filterChildElems(havingType[TaxonomyPackageRef]).collect { case e: TaxonomyPackageRef => e }
    }
  }

  final case class VersioningReportsElem(underlyingElem: BackingNodes.Elem) extends TpElem {

    def findAllVersioningReports: Seq[VersioningReport] = {
      filterChildElems(havingType[VersioningReport]).collect { case e: VersioningReport => e }
    }
  }

  final case class EntryPoint(underlyingElem: BackingNodes.Elem) extends TpElem {

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

  final case class EntryPointDocument(underlyingElem: BackingNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))

    def entryPoint: EntryPoint = {
      findAncestorElemOrSelf(havingType[EntryPoint]).collect { case e: EntryPoint => e }.get
    }
  }

  final case class VersioningReport(underlyingElem: BackingNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))
  }

  final case class LanguagesElem(underlyingElem: BackingNodes.Elem) extends TpElem {

    def findAllLanguages: Seq[Language] = filterChildElems(havingType[Language]).collect { case e: Language => e }
  }

  final case class Language(underlyingElem: BackingNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class TaxonomyPackageRef(underlyingElem: BackingNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  final case class OtherTpElem(underlyingElem: BackingNodes.Elem) extends TpElem

  /**
   * TP text node
   */
  final case class TpText(text: String) extends TpNode with BackingNodes.Text

  /**
   * TP comment node
   */
  final case class TpComment(text: String) extends TpCanBeDocumentChild with BackingNodes.Comment

  /**
   * TP processing instruction node
   */
  final case class TpProcessingInstruction(target: String, data: String) extends TpCanBeDocumentChild with BackingNodes.ProcessingInstruction

  object TpNode {

    def opt(underlyingNode: BackingNodes.Node): Option[TpNode] = {
      underlyingNode match {
        case e: BackingNodes.Elem => Some(TpElem(e))
        case t: BackingNodes.Text => Some(TpText(t.text))
        case c: BackingNodes.Comment => Some(TpComment(c.text))
        case pi: BackingNodes.ProcessingInstruction => Some(TpProcessingInstruction(pi.target, pi.data))
        case _ => None
      }
    }
  }

  object TpElem {

    def apply(underlyingElem: BackingNodes.Elem): TpElem = {
      constructors.get(underlyingElem.name).map(f => f(underlyingElem)).getOrElse(OtherTpElem(underlyingElem))
    }

    // Fast construction using Map taking element name as key

    private val constructors: Map[EName, BackingNodes.Elem => TpElem] = Map(
      TpTaxonomyPackageEName -> { e => new TaxonomyPackage(e) },
      TpIdentifierEName -> { e => Identifier(e) },
      TpVersionEName -> { e => Version(e) },
      TpLicenseEName -> { e => License(e) },
      TpPublisherEName -> { e => Publisher(e) },
      TpPublisherURLEName -> { e => PublisherUrl(e) },
      TpPublisherCountryEName -> { e => PublisherCountry(e) },
      TpPublicationDateEName -> { e => PublicationDate(e) },
      TpEntryPointsEName -> { e => EntryPointsElem(e) },
      TpEntryPointEName -> { e => EntryPoint(e) },
      TpSupersededTaxonomyPackagesEName -> { e => SupersededTaxonomyPackagesElem(e) },
      TpVersioningReportsEName -> { e => VersioningReportsElem(e) },
      TpEntryPointDocumentEName -> { e => EntryPointDocument(e) },
      TpLanguagesEName -> { e => LanguagesElem(e) },
      TpTaxonomyPackageRefEName -> { e => TaxonomyPackageRef(e) },
      TpVersioningReportEName -> { e => VersioningReport(e) },
      TpNameEName -> { e => Name(e) },
      TpDescriptionEName -> { e => Description(e) },
      TpLanguageEName -> { e => Language(e) },
    )
  }

  object TaxonomyPackage {

    def apply(underlyingElem: BackingNodes.Elem): TaxonomyPackage = {
      require(underlyingElem.name == TpTaxonomyPackageEName, s"Expected taxonomy package but got '${underlyingElem.name}'")
      new TaxonomyPackage(underlyingElem)
    }
  }

  /**
   * ElemStep factory API for TP elements.
   */
  object TpElemSteps extends BackingElemStepFactory {

    type ElemType = TpElem
  }

}
