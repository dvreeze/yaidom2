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

package eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxorewriter

import java.net.URI

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.node.indexed
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.queryapi.oo.named
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.ENames
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.Namespaces
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.ResolvedElemTransformations
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.SimpleElemFactory
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.SimpleElemFactory.ScopedQName
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.locatorfreetaxo
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.locatorfreetaxo.TaxonomyElemKey
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxo

/**
 * Transformer of an XBRL taxonomy to its locator-free counterpart. Files are converted one by one to their counterpart.
 * The locator-free taxonomies are not valid XBRL taxonomies, nor are they meant to be. Locator-free taxonomies contain
 * no XLink locators or XLink simple links. XLink locators are replaced by XLink resources with a semantically meaningful
 * element name and element text value.
 *
 * The locator-free taxonomies lean on the 2 core XML schemas for XLink attributes and XLink elements in XBRL. They can
 * contain normal generic link content (like formulas and tables), but the "link" namespace is replaced by a corresponding
 * namespace (no longer XBRL so no longer limited by its requirements on the use of simple links, for example). Also,
 * besides the linkbase root element, generic links are in another namespace. Standard linkbase content is also in another
 * namespace in this representation. Arcroles and roles remain the same as in the original.
 *
 * @author Chris de Vreeze
 */
final class TaxonomyTransformer(val inputTaxonomy: taxo.Taxonomy) {

  // TODO What do we add to schema in order to locally reason about substitution groups?

  import ResolvedElemTransformations._
  import TaxonomyTransformer._

  private val extraScope: Scope = {
    inputTaxonomy.documentMap.values.foldLeft(Scope.Empty) { case (accScope, doc) =>
      accScope ++ doc.documentElement.scope.withoutDefaultNamespace
    }.ensuring(_.defaultNamespaceOption.isEmpty)
  }

  def transformTaxonomy(): locatorfreetaxo.Taxonomy = {
    // TODO Enhance the entrypoints, but for that we need to know the DTSes

    def isXbrlOrgDoc(docUri: URI): Boolean = docUri.toString.contains("www.xbr.org/") // TODO This is shaky
    def isW3OrgDoc(docUri: URI): Boolean = docUri.toString.contains("www.w3.org/") // TODO This is shaky

    val docMap: Map[URI, locatorfreetaxo.TaxonomyDocument] = inputTaxonomy.documentMap.view.mapValues { doc =>
      if (inputTaxonomy.entrypointUris.contains(doc.docUri)) {
        locatorfreetaxo.TaxonomyDocument.build(doc.doc)
      } else if (isXbrlOrgDoc(doc.docUri) || isW3OrgDoc(doc.docUri)) {
        locatorfreetaxo.TaxonomyDocument.build(doc.doc)
      } else {
        println(s"Transforming document '${doc.docUri}'") // Poor man's logging

        doc.documentElement.name match {
          case ENames.XsSchemaEName => transformSchema(doc)
          case ENames.LinkLinkbaseEName => transformLinkbase(doc)
          case _ => locatorfreetaxo.TaxonomyDocument.build(doc.doc)
        }
      }
    }.toMap

    new locatorfreetaxo.Taxonomy(inputTaxonomy.entrypointUris, docMap)
  }

  /**
   * Transforms a schema document. LinkbaseRefs are removed (but have to be added to the entrypoint),
   * and schemaLocation attributes are removed from schema imports.
   *
   * Do not call this method for an entrypoint schema.
   *
   * TODO Transform role types and arcrole types (in the "used" elements).
   *
   * TODO Replace the xbrldt:typedDomainRef attributes by QName-valued attributes.
   */
  def transformSchema(schema: taxo.TaxonomyDocument): locatorfreetaxo.TaxonomyDocument = {
    val schemaElem = schema.documentElement
    require(schemaElem.name == ENames.XsSchemaEName)
    require(
      schemaElem.filterDescendantElems(named(ENames.XsIncludeEName)).isEmpty,
      s"xs:include not allowed in ${schema.docUriOption.getOrElse("")}")

    val resolvedSchemaElem = resolved.Elem.from(schemaElem)

    val resolvedSchemaElemWithoutSchemaLocations = resolvedSchemaElem.transformChildElems { che =>
      if (che.name == ENames.XsImportEName) {
        che.copy(attributes = che.attributes - ENames.SchemaLocationEName)
      } else {
        che
      }
    }

    val editedResolvedSchemaElem = resolvedSchemaElemWithoutSchemaLocations.transformDescendantElemsToNodeSeq { e =>
      if (e.name == ENames.LinkLinkbaseRefEName) {
        Seq.empty
      } else {
        Seq(e)
      }
    }

    val simpleElemFactory = new SimpleElemFactory(extraScope ++ schemaElem.scope)

    val simpleResultElem =
      simpleElemFactory.fromResolvedElem(cleanupEmptyAppinfo(editedResolvedSchemaElem), minimalCScope ++ schemaElem.scope)
    val indexedResultElem = indexed.Elem.ofRoot(schema.docUriOption, simpleResultElem)
    locatorfreetaxo.TaxonomyDocument.build(indexed.Document(indexedResultElem))
  }

  /**
   * Transforms a linkbase document. The most important part of the transformation is the substitution of XLink
   * resources for XLink locators. Also, the root element is linkbase in another (non-XBRL) namespace, and simple
   * links like roleRefs are removed.
   *
   * TODO Consider embedding the linkbase in a schema where the used roleTypes and arcroleTypes are added.
   * TODO Alternatively, repeat the used roleTypes and arcroleTypes in a separate schema very close to the linkbase.
   */
  def transformLinkbase(linkbase: taxo.TaxonomyDocument): locatorfreetaxo.TaxonomyDocument = {
    val linkbaseElem = linkbase.documentElement
    require(linkbaseElem.name == ENames.LinkLinkbaseEName)

    val resolvedLinkbaseElem = resolved.Elem.from(linkbaseElem)

    var scope: Scope = minimalCScope ++ linkbaseElem.scope

    val resultResolvedElem = resolvedLinkbaseElem.copy(name = EName(CLinkNamespace, "linkbase")).transformChildElemsToNodeSeq { che =>
      if (isExtendedLink(che)) {
        val result = transformExtendedLink(che, linkbase.docUri, scope)

        scope = scope ++ result.scope
        Seq(result.elem)
      } else {
        Seq(che).filterNot(_.attrOption(ENames.XLinkTypeEName).contains("simple"))
      }
    }

    val simpleElemFactory = new SimpleElemFactory(extraScope ++ scope)

    val simpleResultElem =
      simpleElemFactory.fromResolvedElem(resultResolvedElem, linkbaseElem.scope ++ scope)
    val indexedResultElem = indexed.Elem.ofRoot(linkbase.docUriOption, simpleResultElem)
    locatorfreetaxo.TaxonomyDocument.build(indexed.Document(indexedResultElem))
  }

  def transformExtendedLink(extendedLink: resolved.Elem, baseUri: URI, scope: Scope): ScopedResolvedElem = {
    import ENames._

    extendedLink.name match {
      case LinkPresentationLinkEName => transformInterConceptExtendedLink(extendedLink, baseUri, scope)
      case LinkDefinitionLinkEName => transformInterConceptExtendedLink(extendedLink, baseUri, scope)
      case LinkCalculationLinkEName => transformInterConceptExtendedLink(extendedLink, baseUri, scope)
      case LinkLabelLinkEName => transformStandardResourceExtendedLink(extendedLink, baseUri, scope)
      case LinkReferenceLinkEName => transformStandardResourceExtendedLink(extendedLink, baseUri, scope)
      case GenLinkEName => transformGenericExtendedLink(extendedLink, baseUri, scope)
      case _ => ScopedResolvedElem(extendedLink, Scope.Empty) // TODO Is this correct? No, the link element namespace is not.
    }
  }

  def transformInterConceptExtendedLink(extendedLink: resolved.Elem, baseUri: URI, startScope: Scope): ScopedResolvedElem = {
    // TODO Handle xml:base attributes in extended link

    require(isExtendedLink(extendedLink), s"Not an extended link: $extendedLink")

    import ENames._

    var scope: Scope = minimalCScope ++ startScope

    val simpleElemFactory = new SimpleElemFactory(extraScope ++ scope)

    val resultElem =
      extendedLink.transformDescendantElemsOrSelf { e =>
        e.name match {
          case nm if Set(LinkPresentationLinkEName, LinkDefinitionLinkEName, LinkCalculationLinkEName).contains(nm) =>
            resolved.Elem(
              mapExtendedLinkElementName(nm),
              e.attributes,
              e.children)
          case nm if Set(LinkPresentationArcEName, LinkDefinitionArcEName, LinkCalculationArcEName).contains(nm) =>
            resolved.Elem(
              mapArcElementName(nm),
              e.attributes,
              e.children)
          case LinkLocEName =>
            val elemUri: URI = baseUri.resolve(e.attr(XLinkHrefEName))
            val conceptName: EName =
              TaxonomyElemKey.getTargetEName(inputTaxonomy.getElem(elemUri).ensuring(_.name == XsElementEName))

            val ScopedQName(conceptQName, addedScope) = simpleElemFactory.convertToQName(conceptName, extraScope ++ scope)
            scope = scope ++ addedScope

            resolved.Node.textElem(
              getLocatorElementNameInStandardLink,
              (e.attributes - XLinkHrefEName) + (XLinkTypeEName -> "resource"),
              conceptQName.toString)
          case _ =>
            e
        }
      }

    ScopedResolvedElem(resultElem, scope)
  }

  def transformStandardResourceExtendedLink(extendedLink: resolved.Elem, baseUri: URI, startScope: Scope): ScopedResolvedElem = {
    // TODO Handle xml:base attributes in extended link

    require(isExtendedLink(extendedLink), s"Not an extended link: $extendedLink")

    import ENames._

    var scope: Scope = minimalCScope ++ startScope

    val simpleElemFactory = new SimpleElemFactory(extraScope ++ scope)

    val resultElem =
      extendedLink.transformDescendantElemsOrSelf { e =>
        e.name match {
          case nm if Set(LinkLabelEName, LinkReferenceLinkEName).contains(nm) =>
            resolved.Elem(
              mapExtendedLinkElementName(nm),
              e.attributes,
              e.children)
          case nm if Set(LinkLabelArcEName, LinkReferenceArcEName).contains(nm) =>
            resolved.Elem(
              mapArcElementName(nm),
              e.attributes,
              e.children)
          case LinkLocEName =>
            // TODO Locators to resources (prohibition/overriding)

            val elemUri: URI = baseUri.resolve(e.attr(XLinkHrefEName))
            val conceptName: EName =
              TaxonomyElemKey.getTargetEName(inputTaxonomy.getElem(elemUri).ensuring(_.name == XsElementEName))

            val ScopedQName(conceptQName, addedScope) = simpleElemFactory.convertToQName(conceptName, extraScope ++ scope)
            scope = scope ++ addedScope

            resolved.Node.textElem(
              getLocatorElementNameInStandardLink,
              (e.attributes - XLinkHrefEName) + (XLinkTypeEName -> "resource"),
              conceptQName.toString)
          case nm if Set(LinkLabelEName, LinkReferenceEName).contains(nm) =>
            resolved.Elem(
              mapResourceElementName(nm),
              e.attributes,
              e.children)
          case _ =>
            e
        }
      }

    ScopedResolvedElem(resultElem, scope)
  }

  def transformGenericExtendedLink(extendedLink: resolved.Elem, baseUri: URI, startScope: Scope): ScopedResolvedElem = {
    // TODO Handle xml:base attributes in extended link

    require(isExtendedLink(extendedLink), s"Not an extended link: $extendedLink")

    import ENames._

    var scope: Scope = minimalCScope ++ startScope

    val simpleElemFactory = new SimpleElemFactory(extraScope ++ scope)

    val resultElem =
      extendedLink.transformDescendantElemsOrSelf { e =>
        e.name match {
          case LinkLocEName =>
            // TODO Locators to resources (prohibition/overriding)

            val elemUri: URI = baseUri.resolve(e.attr(XLinkHrefEName))
            val locatedElem: BackingNodes.Elem = inputTaxonomy.getElem(elemUri)

            val key: TaxonomyElemKey = TaxonomyElemKey.from(locatedElem)

            val keyAsString: String = key match {
              case TaxonomyElemKey.ElementDeclaration(nm) =>
                val ScopedQName(qname, addedScope) = simpleElemFactory.convertToQName(nm, extraScope)
                scope = scope ++ addedScope

                qname.toString
              case TaxonomyElemKey.NamedType(nm) =>
                val ScopedQName(qname, addedScope) = simpleElemFactory.convertToQName(nm, extraScope)
                scope = scope ++ addedScope

                qname.toString
              case TaxonomyElemKey.RoleType(uri) => uri
              case TaxonomyElemKey.ArcroleType(uri) => uri
              case TaxonomyElemKey.EnumerationValue(id) => id
              case TaxonomyElemKey.Id(id) => id
            }

            resolved.Node.textElem(
              mapLocatorElementNameInGenericLink(e.name, key),
              (e.attributes - XLinkHrefEName) + (XLinkTypeEName -> "resource"),
              keyAsString)
          case _ =>
            e
        }
      }

    ScopedResolvedElem(resultElem, scope)
  }

  def isExtendedLink(elem: resolved.Elem): Boolean = {
    elem.attrOption(ENames.XLinkTypeEName).contains("extended")
  }

  def isArc(elem: resolved.Elem): Boolean = {
    elem.attrOption(ENames.XLinkTypeEName).contains("arc")
  }

  private def cleanupEmptyAppinfo(elem: resolved.Elem): resolved.Elem = {
    transformDescendantElemsToNodeSeq(elem, { e =>
      if (e.name == ENames.XsAnnotationEName && e.findAllDescendantElems().size == 1) {
        Seq.empty
      } else {
        Seq(e)
      }
    })
  }
}

object TaxonomyTransformer {

  case class ScopedResolvedElem(elem: resolved.Elem, scope: Scope)

  def mapExtendedLinkElementName(name: EName): EName = {
    name match {
      case ENames.LinkPresentationLinkEName => EName(CLinkNamespace, "presentationLink")
      case ENames.LinkDefinitionLinkEName => EName(CLinkNamespace, "definitionLink")
      case ENames.LinkCalculationLinkEName => EName(CLinkNamespace, "calculationLink")
      case ENames.LinkLabelLinkEName => EName(CLinkNamespace, "labelLink")
      case ENames.LinkReferenceLinkEName => EName(CLinkNamespace, "referenceLink")
      case _ => name
    }
  }

  def mapArcElementName(name: EName): EName = {
    name match {
      case ENames.LinkPresentationArcEName => EName(CLinkNamespace, "presentationArc")
      case ENames.LinkDefinitionArcEName => EName(CLinkNamespace, "definitionArc")
      case ENames.LinkCalculationArcEName => EName(CLinkNamespace, "calculationArc")
      case ENames.LinkLabelArcEName => EName(CLinkNamespace, "labelArc")
      case ENames.LinkReferenceArcEName => EName(CLinkNamespace, "referenceArc")
      case _ => name
    }
  }

  def getLocatorElementNameInStandardLink: EName = {
    EName(CLinkNamespace, "concept")
  }

  def mapLocatorElementNameInGenericLink(name: EName, key: TaxonomyElemKey): EName = {
    name match {
      case ENames.LinkLocEName =>
        key match {
          case TaxonomyElemKey.ElementDeclaration(name) => EName(CLinkNamespace, "elementDecl")
          case TaxonomyElemKey.NamedType(name) => EName(CLinkNamespace, "namedType")
          case TaxonomyElemKey.RoleType(uri) => EName(CLinkNamespace, "roleType")
          case TaxonomyElemKey.ArcroleType(uri) => EName(CLinkNamespace, "arcroleType")
          case TaxonomyElemKey.EnumerationValue(id) => EName(CLinkNamespace, "enumerationValue")
          case TaxonomyElemKey.Id(id) => EName(CLinkNamespace, "id")
        }
      case _ => name // TODO Correct?
    }
  }

  def mapResourceElementName(name: EName): EName = {
    name match {
      case ENames.LinkLabelEName => EName(CLinkNamespace, "label")
      case ENames.LinkReferenceEName => EName(CLinkNamespace, "reference")
      case _ => name
    }
  }

  val sourceSite: URI = new URI("http://www.xbrl.org/").ensuring(_.getPath == "/")
  val dimSourceSite: URI = new URI("http://xbrl.org/").ensuring(_.getPath == "/")

  val targetSite: URI = new URI("http://www.concisexbrl.org/").ensuring(_.getPath == "/")

  val CLinkNamespace: String = "http://www.concisexbrl.org/2003/linkbase"

  val CLinkPrefix: String = "clink"

  val minimalCScope: Scope =
    Scope.from(
      CLinkPrefix -> CLinkNamespace,
      "link" -> Namespaces.LinkNamespace,
      "xlink" -> Namespaces.XLinkNamespace,
      "gen" -> Namespaces.GenNamespace
    )
}
