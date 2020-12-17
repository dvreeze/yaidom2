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

import eu.cdevreeze.yaidom2.core._
import eu.cdevreeze.yaidom2.dialect.AbstractDialectBackingElem
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi._
import eu.cdevreeze.yaidom2.queryapi.elemstep.BackingElemStepFactory
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractSubtypeAwareElem
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.reflect.classTag

/**
 * Query test using a "yaidom dialect" (for so-called XBRL Taxonomy Packages).
 *
 * This test shows how "yaidom dialects" and underlying element implementations are decoupled and can be combined
 * at runtime in any way that is desired.
 *
 * @author Chris de Vreeze
 */
trait TpDialectOverBackingElemQueryTest extends AnyFunSuite {

  import TpDialectOverBackingElemQueryTest._

  protected def document: BackingDocumentApi

  private def rootElem: BackingNodes.Elem = document.documentElement

  test("testQueryEntrypoints") {
    val taxoPackage = TaxonomyPackage(rootElem)

    assertResult(
      rootElem
        .filterDescendantElems(named(TpEntryPointEName))
        .flatMap(_.filterChildElems(named(TpNameEName)))
        .map(_.text)
        .toSet) {

      taxoPackage.findAllEntryPoints.flatMap(_.findAllNames).map(_.value).toSet
    }
  }

  test("testEquivalenceOfEntrypointQueries") {
    val taxoPackage = TaxonomyPackage(rootElem)

    assertResult(
      rootElem
        .filterDescendantElems(named(TpEntryPointEName))
        .map(e => resolved.Elem.from(e))) {

      taxoPackage.findAllEntryPoints.map(e => resolved.Elem.from(e))
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

    val docs = taxoPackage.findAllEntryPoints.flatMap(_.findAllEntryPointDocuments)

    assertResult(true) {
      docs.size > 10
    }
    assertResult(taxoPackage.filterDescendantElems(named(TpEntryPointDocumentEName))) {
      docs
    }
  }

  test("testResolvedElemProperty") {
    val taxonomyPackage = TaxonomyPackage(rootElem)

    assertResult(resolved.Elem.from(taxonomyPackage).findAllDescendantElemsOrSelf) {
      taxonomyPackage.findAllDescendantElemsOrSelf.map(e => resolved.Elem.from(e))
    }
  }

  test("testQueryEntrypointOfEntrypointDocuments") {
    val taxoPackage = TaxonomyPackage(rootElem)

    val docs = taxoPackage.findAllEntryPoints.flatMap(_.findAllEntryPointDocuments)

    assertResult(true) {
      docs.size > 10
    }
    assertResult(taxoPackage.filterDescendantElems(named(TpEntryPointDocumentEName))) {
      docs
    }

    assertResult(taxoPackage.findAllEntryPoints) {
      docs.map(_.entryPoint).distinct
    }
  }

  test("testQueryRootOfAllDescendantElemsOrSelf") {
    val taxoPackage = TaxonomyPackage(rootElem)

    assertResult(Seq(taxoPackage)) {
      taxoPackage.findAllDescendantElemsOrSelf.map(_.taxonomyPackage).distinct
    }
  }

  test("testResolvedElemPropertyViaDocument") {
    val taxonomyPackageDoc = TpDocument(document)

    assertResult(resolved.Elem.from(taxonomyPackageDoc.documentElement).findAllDescendantElemsOrSelf) {
      taxonomyPackageDoc.documentElement.findAllDescendantElemsOrSelf.map(e => resolved.Elem.from(e))
    }
  }
}

object TpDialectOverBackingElemQueryTest {

  // First the general yaidom dialect support

  def havingType[T: ClassTag]: ClarkNodes.Elem => Boolean = {
    {
      case e: T => true
      case e    => false
    }
  }

  def where[T: ClassTag](p: T => Boolean): ClarkNodes.Elem => Boolean = {
    {
      case e: T if p(e) => true
      case e            => false
    }
  }

  // Below is the TP (Taxonomy Package) specific dialect support

  val TpNs = "http://xbrl.org/2016/taxonomy-package"

  val TpTaxonomyPackageEName: EName = EName(TpNs, "taxonomyPackage")
  val TpIdentifierEName: EName = EName(TpNs, "identifier")
  val TpVersionEName: EName = EName(TpNs, "version")
  val TpLicenseEName: EName = EName(TpNs, "license")
  val TpPublisherEName: EName = EName(TpNs, "publisher")
  val TpPublisherURLEName: EName = EName(TpNs, "publisherURL")
  val TpPublisherCountryEName: EName = EName(TpNs, "publisherCountry")
  val TpPublicationDateEName: EName = EName(TpNs, "publicationDate")
  val TpEntryPointsEName: EName = EName(TpNs, "entryPoints")
  val TpEntryPointEName: EName = EName(TpNs, "entryPoint")
  val TpSupersededTaxonomyPackagesEName: EName = EName(TpNs, "supersededTaxonomyPackages")
  val TpVersioningReportsEName: EName = EName(TpNs, "versioningReports")
  val TpEntryPointDocumentEName: EName = EName(TpNs, "entryPointDocument")
  val TpLanguagesEName: EName = EName(TpNs, "languages")
  val TpTaxonomyPackageRefEName: EName = EName(TpNs, "taxonomyPackageRef")
  val TpVersioningReportEName: EName = EName(TpNs, "versioningReport")
  val TpNameEName: EName = EName(TpNs, "name")
  val TpDescriptionEName: EName = EName(TpNs, "description")
  val TpLanguageEName: EName = EName(TpNs, "language")

  val HrefEName: EName = e"href"
  val NameEName: EName = e"name"

  final case class TpDocument(underlyingDoc: BackingDocumentApi) extends DocumentApi {
    require(underlyingDoc.documentElement.name == TpTaxonomyPackageEName, s"Expected TaxonomyPackage document element")

    type NodeType = TpNode

    type CanBeDocumentChildType = TpCanBeDocumentChild

    type ElemType = TpElem

    def docUriOption: Option[URI] = documentElement.docUriOption

    def documentElement: TaxonomyPackage = TaxonomyPackage(underlyingDoc.documentElement)

    def children: Seq[TpCanBeDocumentChild] = {
      underlyingDoc.children.map {
        case e: BackingNodes.Elem                   => documentElement
        case c: BackingNodes.Comment                => TpComment(c.text)
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
  sealed trait TpElem extends AbstractDialectBackingElem with AbstractSubtypeAwareElem with TpCanBeDocumentChild {

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

    override def findAllChildElems: Seq[ThisElem] = super.findAllChildElems

    override def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = super.filterDescendantElems(p)

    override def findAllDescendantElems: Seq[ThisElem] = super.findAllDescendantElems

    override def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = super.findDescendantElem(p)

    override def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.filterDescendantElemsOrSelf(p)

    override def findAllDescendantElemsOrSelf: Seq[ThisElem] = super.findAllDescendantElemsOrSelf

    override def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = super.findDescendantElemOrSelf(p)

    override def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = super.findTopmostElems(p)

    override def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.findTopmostElemsOrSelf(p)

    override def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = super.getDescendantElemOrSelf(navigationPath)

    override def findParentElem: Option[ThisElem] = super.findParentElem

    override def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = super.filterAncestorElems(p)

    override def findAllAncestorElems: Seq[ThisElem] = super.findAllAncestorElems

    override def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = super.findAncestorElem(p)

    override def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.filterAncestorElemsOrSelf(p)

    override def findAllAncestorElemsOrSelf: Seq[ThisElem] = super.findAllAncestorElemsOrSelf

    override def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = super.findAncestorElemOrSelf(p)

    override def findAllPrecedingSiblingElems: Seq[ThisElem] = super.findAllPrecedingSiblingElems

    override def ownNavigationPathRelativeToRootElem: Seq[Int] = super.ownNavigationPathRelativeToRootElem

    override def filterChildElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] =
      super.filterChildElemsOfType(subtype)(p)

    override def findAllChildElemsOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B] =
      super.findAllChildElemsOfType(subtype)

    override def findChildElemOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B] =
      super.findChildElemOfType(subtype)(p)

    override def findFirstChildElemOfType[B <: ThisElem](subtype: ClassTag[B]): Option[B] =
      super.findFirstChildElemOfType(subtype)

    override def filterDescendantElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] =
      super.filterDescendantElemsOfType(subtype)(p)

    override def findAllDescendantElemsOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B] =
      super.findAllDescendantElemsOfType(subtype)

    override def findDescendantElemOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B] =
      super.findDescendantElemOfType(subtype)(p)

    override def filterDescendantElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] =
      super.filterDescendantElemsOrSelfOfType(subtype)(p)

    override def findAllDescendantElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B] =
      super.findAllDescendantElemsOrSelfOfType(subtype)

    override def findDescendantElemOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B] =
      super.findDescendantElemOrSelfOfType(subtype)(p)

    override def findTopmostElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] =
      super.findTopmostElemsOfType(subtype)(p)

    override def findTopmostElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] =
      super.findTopmostElemsOrSelfOfType(subtype)(p)
  }

  final case class TaxonomyPackage(underlyingElem: BackingNodes.Elem) extends TpElem {

    def findAllEntryPoints: Seq[EntryPoint] = {
      findAllDescendantElemsOfType(classTag[EntryPoint])
    }

    def filterEntryPoints(p: EntryPoint => Boolean): Seq[EntryPoint] = {
      filterDescendantElemsOfType(classTag[EntryPoint])(p)
    }

    def findEntryPoint(p: EntryPoint => Boolean): Option[EntryPoint] = {
      findDescendantElemOfType(classTag[EntryPoint])(p)
    }

    def getEntryPoint(p: EntryPoint => Boolean): EntryPoint = {
      findDescendantElemOfType(classTag[EntryPoint])(p)
        .getOrElse(sys.error(s"Missing entryPoint obeying the given predicate"))
    }

    // Child elements

    def getIdentifier: Identifier = {
      findFirstChildElemOfType(classTag[Identifier]).get
    }

    def findAllDocumentationGroups: Seq[DocumentationGroup] = {
      findAllChildElemsOfType(classTag[DocumentationGroup])
    }

    def findAllNames: Seq[Name] = {
      findAllChildElemsOfType(classTag[Name])
    }

    def findAllDescriptions: Seq[Description] = {
      findAllChildElemsOfType(classTag[Description])
    }

    def findVersion: Option[Version] = {
      findFirstChildElemOfType(classTag[Version])
    }

    def findLicense: Option[License] = {
      findFirstChildElemOfType(classTag[License])
    }

    def findAllPublishers: Seq[Publisher] = {
      findAllChildElemsOfType(classTag[Publisher])
    }

    def findPublisherUrl: Option[PublisherUrl] = {
      findFirstChildElemOfType(classTag[PublisherUrl])
    }

    def findPublisherCountry: Option[PublisherCountry] = {
      findFirstChildElemOfType(classTag[PublisherCountry])
    }

    def findPublicationDate: Option[PublicationDate] = {
      findFirstChildElemOfType(classTag[PublicationDate])
    }

    def findEntryPointsElem: Option[EntryPointsElem] = {
      findFirstChildElemOfType(classTag[EntryPointsElem])
    }

    def findSupersededTaxonomyPackagesElem: Option[SupersededTaxonomyPackagesElem] = {
      findFirstChildElemOfType(classTag[SupersededTaxonomyPackagesElem])
    }

    def findVersioningReportsElem: Option[VersioningReportsElem] = {
      findFirstChildElemOfType(classTag[VersioningReportsElem])
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
      findAllChildElemsOfType(classTag[EntryPoint])
    }
  }

  final case class SupersededTaxonomyPackagesElem(underlyingElem: BackingNodes.Elem) extends TpElem {

    def findAllTaxonomyPackageRefs: Seq[TaxonomyPackageRef] = {
      findAllChildElemsOfType(classTag[TaxonomyPackageRef])
    }
  }

  final case class VersioningReportsElem(underlyingElem: BackingNodes.Elem) extends TpElem {

    def findAllVersioningReports: Seq[VersioningReport] = {
      findAllChildElemsOfType(classTag[VersioningReport])
    }
  }

  final case class EntryPoint(underlyingElem: BackingNodes.Elem) extends TpElem {

    def findAllEntryPointHrefs: Seq[URI] = {
      findAllEntryPointDocuments.map(_.href)
    }

    def findAllDocumentationGroups: Seq[DocumentationGroup] = {
      findAllChildElemsOfType(classTag[DocumentationGroup])
    }

    def findAllNames: Seq[Name] = {
      findAllChildElemsOfType(classTag[Name])
    }

    def findAllDescriptions: Seq[Description] = {
      findAllChildElemsOfType(classTag[Description])
    }

    def findVersion: Option[Version] = {
      findFirstChildElemOfType(classTag[Version])
    }

    def findAllEntryPointDocuments: Seq[EntryPointDocument] = {
      findAllChildElemsOfType(classTag[EntryPointDocument])
    }

    def findLanguagesElem: Option[LanguagesElem] = {
      findFirstChildElemOfType(classTag[LanguagesElem])
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

    def findAllLanguages: Seq[Language] = findAllChildElemsOfType(classTag[Language])
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
  final case class TpProcessingInstruction(target: String, data: String)
      extends TpCanBeDocumentChild
      with BackingNodes.ProcessingInstruction

  object TpNode {

    def opt(underlyingNode: BackingNodes.Node): Option[TpNode] = {
      underlyingNode match {
        case e: BackingNodes.Elem                   => Some(TpElem(e))
        case t: BackingNodes.Text                   => Some(TpText(t.text))
        case c: BackingNodes.Comment                => Some(TpComment(c.text))
        case pi: BackingNodes.ProcessingInstruction => Some(TpProcessingInstruction(pi.target, pi.data))
        case _                                      => None
      }
    }
  }

  object TpElem {

    def apply(underlyingElem: BackingNodes.Elem): TpElem = {
      constructors.get(underlyingElem.name).map(f => f(underlyingElem)).getOrElse(OtherTpElem(underlyingElem))
    }

    // Fast construction using Map taking element name as key

    private val constructors: Map[EName, BackingNodes.Elem => TpElem] = Map(
      TpTaxonomyPackageEName -> { e =>
        new TaxonomyPackage(e)
      },
      TpIdentifierEName -> { e =>
        Identifier(e)
      },
      TpVersionEName -> { e =>
        Version(e)
      },
      TpLicenseEName -> { e =>
        License(e)
      },
      TpPublisherEName -> { e =>
        Publisher(e)
      },
      TpPublisherURLEName -> { e =>
        PublisherUrl(e)
      },
      TpPublisherCountryEName -> { e =>
        PublisherCountry(e)
      },
      TpPublicationDateEName -> { e =>
        PublicationDate(e)
      },
      TpEntryPointsEName -> { e =>
        EntryPointsElem(e)
      },
      TpEntryPointEName -> { e =>
        EntryPoint(e)
      },
      TpSupersededTaxonomyPackagesEName -> { e =>
        SupersededTaxonomyPackagesElem(e)
      },
      TpVersioningReportsEName -> { e =>
        VersioningReportsElem(e)
      },
      TpEntryPointDocumentEName -> { e =>
        EntryPointDocument(e)
      },
      TpLanguagesEName -> { e =>
        LanguagesElem(e)
      },
      TpTaxonomyPackageRefEName -> { e =>
        TaxonomyPackageRef(e)
      },
      TpVersioningReportEName -> { e =>
        VersioningReport(e)
      },
      TpNameEName -> { e =>
        Name(e)
      },
      TpDescriptionEName -> { e =>
        Description(e)
      },
      TpLanguageEName -> { e =>
        Language(e)
      },
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
