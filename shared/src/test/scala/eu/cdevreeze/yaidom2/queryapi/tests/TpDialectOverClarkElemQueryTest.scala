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
import eu.cdevreeze.yaidom2.dialect.AbstractDialectClarkElem
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi._
import eu.cdevreeze.yaidom2.queryapi.elemstep.ClarkElemStepFactory
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
trait TpDialectOverClarkElemQueryTest extends AnyFunSuite {

  import TpDialectOverClarkElemQueryTest._

  protected def document: ClarkDocumentApi

  private def rootElem: ClarkNodes.Elem = document.documentElement

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

  test("testResolvedElemPropertyViaDocument") {
    val taxonomyPackageDoc = TpDocument(document)

    assertResult(resolved.Elem.from(taxonomyPackageDoc.documentElement).findAllDescendantElemsOrSelf) {
      taxonomyPackageDoc.documentElement.findAllDescendantElemsOrSelf.map(e => resolved.Elem.from(e))
    }
  }
}

object TpDialectOverClarkElemQueryTest {

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
  sealed trait TpElem extends AbstractDialectClarkElem with AbstractSubtypeAwareElem with TpCanBeDocumentChild {

    type ThisElem = TpElem

    type ThisNode = TpNode

    final def wrapElem(underlyingElem: ClarkNodes.Elem): ThisElem = TpElem(underlyingElem)

    // ClarkNodes.Elem

    final def children: ArraySeq[TpNode] = {
      underlyingElem.children.flatMap(TpNode.opt).to(ArraySeq)
    }

    final override def select(step: ElemStep[TpElem]): Seq[TpElem] = {
      step(this)
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

  final case class TaxonomyPackage(underlyingElem: ClarkNodes.Elem) extends TpElem {

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

  final case class Identifier(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  sealed trait DocumentationGroup extends TpElem {

    def value: String
  }

  final case class Name(underlyingElem: ClarkNodes.Elem) extends TpElem with DocumentationGroup {

    def value: String = text
  }

  final case class Description(underlyingElem: ClarkNodes.Elem) extends TpElem with DocumentationGroup {

    def value: String = text
  }

  final case class Version(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class License(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))

    def licenseName: String = attr(NameEName)

    def value: String = text
  }

  final case class Publisher(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class PublisherUrl(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  final case class PublisherCountry(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class PublicationDate(underlyingElem: ClarkNodes.Elem) extends TpElem {

    // Ignoring time zones, because dates without times are unlikely to contain time zones.
    def value: LocalDate = LocalDate.parse(text)
  }

  final case class EntryPointsElem(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def findAllEntryPoints: Seq[EntryPoint] = {
      findAllChildElemsOfType(classTag[EntryPoint])
    }
  }

  final case class SupersededTaxonomyPackagesElem(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def findAllTaxonomyPackageRefs: Seq[TaxonomyPackageRef] = {
      findAllChildElemsOfType(classTag[TaxonomyPackageRef])
    }
  }

  final case class VersioningReportsElem(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def findAllVersioningReports: Seq[VersioningReport] = {
      findAllChildElemsOfType(classTag[VersioningReport])
    }
  }

  final case class EntryPoint(underlyingElem: ClarkNodes.Elem) extends TpElem {

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

  final case class EntryPointDocument(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))
  }

  final case class VersioningReport(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def href: URI = URI.create(attr(HrefEName))
  }

  final case class LanguagesElem(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def findAllLanguages: Seq[Language] = findAllChildElemsOfType(classTag[Language])
  }

  final case class Language(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def value: String = text
  }

  final case class TaxonomyPackageRef(underlyingElem: ClarkNodes.Elem) extends TpElem {

    def value: URI = URI.create(text)
  }

  final case class OtherTpElem(underlyingElem: ClarkNodes.Elem) extends TpElem

  /**
   * TP text node
   */
  final case class TpText(text: String) extends TpNode with ClarkNodes.Text

  object TpNode {

    def opt(underlyingNode: ClarkNodes.Node): Option[TpNode] = {
      underlyingNode match {
        case e: ClarkNodes.Elem => Some(TpElem(e))
        case t: ClarkNodes.Text => Some(TpText(t.text))
        case _                  => None
      }
    }
  }

  object TpElem {

    def apply(underlyingElem: ClarkNodes.Elem): TpElem = {
      constructors.get(underlyingElem.name).map(f => f(underlyingElem)).getOrElse(OtherTpElem(underlyingElem))
    }

    // Fast construction using Map taking element name as key

    private val constructors: Map[EName, ClarkNodes.Elem => TpElem] = Map(
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
