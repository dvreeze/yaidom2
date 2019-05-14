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

package eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.locatorfreetaxo

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi.oo.named
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.ENames
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.Namespaces
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.ScopedQName
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.ScopedResolvedElem
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.SimpleElemFactory

/**
 * A conceptual locator, implemented as an XLink resource holding a semantically meaningful key.
 *
 * @author Chris de Vreeze
 */
sealed trait TaxonomyElemKey {

  type ValueType

  /**
   * Returns the element name of the corresponding XLink resources in an extended link. This name is also a unique
   * identifier of the kind of "locator".
   */
  def resourceElementName: EName

  def value: ValueType

  /**
   * Creates an XLink resource element for this taxonomy element key. The element name is the result of function resourceElementName.
   */
  def convertToResolvedElem(xlinkLabel: String, knownScope: Scope): ScopedResolvedElem
}

sealed trait NamedSchemaElementKey extends TaxonomyElemKey {

  type ValueType = EName

  def name: EName = value

  final def convertToResolvedElem(xlinkLabel: String, knownScope: Scope): ScopedResolvedElem = {
    val simpleElemFactory = new SimpleElemFactory(knownScope)

    val ScopedQName(qname, addedScope) = simpleElemFactory.convertToQName(name, knownScope)

    ScopedResolvedElem(
      resolved.Node.textElem(
        resourceElementName,
        SeqMap(ENames.XLinkTypeEName -> "resource", ENames.XLinkLabelEName -> xlinkLabel),
        qname.toString),
      addedScope)
  }
}

object TaxonomyElemKey {

  import Namespaces._

  final case class Concept(value: EName) extends NamedSchemaElementKey {

    def resourceElementName: EName = EName(CLinkNamespace, "concept")
  }

  final case class ElementDeclaration(value: EName) extends NamedSchemaElementKey {

    def resourceElementName: EName = EName(CLinkNamespace, "elementDeclaration")
  }

  final case class RoleType(uri: String) extends TaxonomyElemKey {

    type ValueType = String

    def resourceElementName: EName = EName(CLinkNamespace, "roleType")

    def value: String = uri

    def convertToResolvedElem(xlinkLabel: String, knownScope: Scope): ScopedResolvedElem = {
      ScopedResolvedElem(
        resolved.Node.textElem(
          resourceElementName,
          SeqMap(ENames.XLinkTypeEName -> "resource", ENames.XLinkLabelEName -> xlinkLabel),
          uri),
        Scope.Empty)
    }
  }

  final case class ArcroleType(uri: String) extends TaxonomyElemKey {

    type ValueType = String

    def resourceElementName: EName = EName(CLinkNamespace, "arcroleType")

    def value: String = uri

    def convertToResolvedElem(xlinkLabel: String, knownScope: Scope): ScopedResolvedElem = {
      ScopedResolvedElem(
        resolved.Node.textElem(
          resourceElementName,
          SeqMap(ENames.XLinkTypeEName -> "resource", ENames.XLinkLabelEName -> xlinkLabel),
          uri),
        Scope.Empty)
    }
  }

  final case class NamedType(value: EName) extends NamedSchemaElementKey {

    def resourceElementName: EName = EName(CLinkNamespace, "namedType")
  }

  final case class UniqueIdInSchema(id: IdInSchema) extends TaxonomyElemKey {

    type ValueType = IdInSchema

    def resourceElementName: EName = EName(CLinkNamespace, "idInSchema")

    def value: IdInSchema = id

    def convertToResolvedElem(xlinkLabel: String, knownScope: Scope): ScopedResolvedElem = {
      ScopedResolvedElem(
        resolved.Node.textElem(
          resourceElementName,
          SeqMap(
            ENames.XLinkTypeEName -> "resource",
            ENames.XLinkLabelEName -> xlinkLabel,
            EName(None, "namespace") -> id.targetNamespace),
          id.id),
        Scope.Empty)
    }
  }

  object UniqueIdInSchema {

    def apply(id: String, targetNamespace: String): UniqueIdInSchema = UniqueIdInSchema(IdInSchema(id, targetNamespace))
  }

  final case class UniqueIdInLink(id: IdInLink) extends TaxonomyElemKey {

    type ValueType = IdInLink

    def resourceElementName: EName = EName(CLinkNamespace, "idInLink")

    def value: IdInLink = id

    def convertToResolvedElem(xlinkLabel: String, knownScope: Scope): ScopedResolvedElem = {
      val simpleElemFactory = new SimpleElemFactory(knownScope)

      val ScopedQName(extLinkQName, addedScope) = simpleElemFactory.convertToQName(id.extendedLinkName, knownScope)

      ScopedResolvedElem(
        resolved.Node.textElem(
          resourceElementName,
          SeqMap(
            ENames.XLinkTypeEName -> "resource",
            ENames.XLinkLabelEName -> xlinkLabel,
            EName(None, "linkName") -> extLinkQName.toString,
            EName(None, "linkRole") -> id.extendedLinkrole),
          id.id),
        addedScope)
    }
  }

  object UniqueIdInLink {

    def apply(id: String, extendedLinkName: EName, extendedLinkrole: String): UniqueIdInLink = {
      UniqueIdInLink(IdInLink(id, extendedLinkName, extendedLinkrole))
    }
  }

  final case class IdInSchema(id: String, targetNamespace: String)

  final case class IdInLink(id: String, extendedLinkName: EName, extendedLinkrole: String)

  /**
   * Converts the given (referred) element in an XBRL taxonomy to a taxonomy element key. For an element declaration,
   * the element declaration key is chosen and not the concept key, since the substitution group is unknown from the XML
   * element name alone.
   */
  def fromReferredElement(elem: BackingNodes.Elem): TaxonomyElemKey = {
    import ENames._

    elem.name match {
      case XsElementEName => ElementDeclaration(getTargetEName(elem))
      case LinkRoleTypeEName => RoleType(elem.attr(RoleURIEName))
      case LinkArcroleTypeEName => ArcroleType(elem.attr(ArcroleURIEName))
      case XsComplexTypeEName => NamedType(getTargetEName(elem))
      case XsSimpleTypeEName => NamedType(getTargetEName(elem))
      case _ if elem.findAncestorElemOrSelf(named(LinkLinkbaseEName)).nonEmpty =>
        require(elem.attrOption(IdEName).nonEmpty, s"Missing ID on ${elem.name}")

        val extLink = elem.findAncestorElemOrSelf(named(LinkLinkbaseEName)).get

        UniqueIdInLink(elem.attr(IdEName), extLink.name, extLink.attr(XLinkRoleEName))
      case _ =>
        require(elem.attrOption(IdEName).nonEmpty, s"Missing ID on ${elem.name}")
        require(elem.findAncestorElemOrSelf(named(XsSchemaEName)).nonEmpty, s"Missing ancestor-of-self schema element")

        val schema = elem.findAncestorElemOrSelf(named(XsSchemaEName)).get

        UniqueIdInSchema(elem.attr(IdEName), schema.attrOption(TargetNamespaceEName).getOrElse(""))
    }
  }

  /**
   * Returns the target namespace of the schema element, which is assumed to be something like a global element declaration
   * or a named type definition.
   */
  def getTargetEName(elem: BackingNodes.Elem): EName = {
    require(elem.attrOption(ENames.NameEName).nonEmpty, s"Missing name attribute on ${elem.name}")

    val tnsOption: Option[String] =
      elem.findAncestorElemOrSelf(_.name == ENames.XsSchemaEName).flatMap(_.attrOption(ENames.TargetNamespaceEName))

    EName(tnsOption, elem.attr(ENames.NameEName))
  }
}
