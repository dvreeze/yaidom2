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

import java.net.URI

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.ENames
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.ScopedQName
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.ScopedResolvedElem
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.SimpleElemFactory
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.xpointer.XPointer

/**
 * Unique key of a taxonomy element, replacing XLink locators. The taxonomy element key is in XML an XLink resource,
 * with the intent of referring to another XML element in the taxonomy.
 *
 * Such keys can refer to "normal" schema components, such as element declarations, named type definitions, attribute
 * declarations etc. An important special case of element declarations is concept declarations.
 *
 * They can also refer to "appinfo" content, such as role types and arcrole types.
 *
 * Finally they can refer to linkbase content, typically XLink resources (such as concept labels etc.).
 *
 * Preferably the keys are stable, even when roundtripping between XBRL taxonomies and locator-free taxonomies.
 * Preferably the keys also convey the kind of taxonomy element referred to, such as a concept or role type.
 *
 * The hierarchy of taxonomy element keys cannot be extended by user code (for now?).
 *
 * @author Chris de Vreeze
 */
sealed trait TaxonomyElemKey {

  type ValueType

  /**
   * Returns the element name of the corresponding XLink resources in an extended link. This name is typically also a unique
   * identifier of the kind of key.
   */
  def resourceElementName: EName

  def value: ValueType

  /**
   * Creates an XLink resource element for this taxonomy element key. The element name is the result of function resourceElementName.
   */
  def convertToResolvedElem(xlinkLabel: String, knownScope: Scope): ScopedResolvedElem
}

/**
 * Key referring to a top level schema component declaration or definition.
 */
sealed trait SchemaComponentKey extends TaxonomyElemKey {

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

/**
 * Key referring to an element inside an xs:appinfo section, such as a role type definition.
 */
sealed trait CustomSchemaContentKey extends TaxonomyElemKey

/**
 * Key referring to an element that is not a schema component or xs:appinfo content.
 * In almost all case this element is therefore linkbase content, typically an XLink resource.
 * It could also be the schema root element, for example.
 *
 * TODO Make this type extensible by user code.
 */
sealed trait AnyTaxonomyElemKey extends TaxonomyElemKey

object TaxonomyElemKey {

  import ENames._

  final case class ConceptKey(value: EName) extends SchemaComponentKey {

    def resourceElementName: EName = CKeyConceptKeyEName
  }

  final case class ElementKey(value: EName) extends SchemaComponentKey {

    def resourceElementName: EName = CKeyElementKeyEName
  }

  final case class RoleKey(uri: String) extends CustomSchemaContentKey {

    type ValueType = String

    def resourceElementName: EName = CKeyRoleKeyEName

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

  final case class ArcroleKey(uri: String) extends CustomSchemaContentKey {

    type ValueType = String

    def resourceElementName: EName = CKeyArcroleKeyEName

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

  final case class TypeKey(value: EName) extends SchemaComponentKey {

    def resourceElementName: EName = CKeyTypeKeyEName
  }

  /**
   * URI key for any taxonomy element. The URI must be absolute and must have an XPointer fragment. When using this key, we are
   * back to XLink locators again, be it with a different syntax. Therefore prefer the other keys, whenever possible.
   *
   * If this key class is used, by all means use shorthand pointers (i.e. IDs) as fragments. After all, the document URI
   * is stable (survives roundtripping from and to XBRL taxonomies), and the ID is stable inside the document as well.
   * If element scheme pointers are used, they will typically break when converting to XBRL taxonomies.
   */
  final case class AnyElemKey(uri: URI) extends AnyTaxonomyElemKey {
    require(uri.isAbsolute, s"Expected absolute URI, but got URI '$uri' instead")
    require(uri.getFragment != null, s"Expected non-empty URI fragment, but got URI '$uri' instead")

    type ValueType = String

    def resourceElementName: EName = CKeyAnyElemKeyEName

    def value: String = uri.toString

    def convertToResolvedElem(xlinkLabel: String, knownScope: Scope): ScopedResolvedElem = {
      ScopedResolvedElem(
        resolved.Node.textElem(
          resourceElementName,
          SeqMap(
            ENames.XLinkTypeEName -> "resource",
            ENames.XLinkLabelEName -> xlinkLabel),
          uri.toString),
        Scope.Empty)
    }

    def docUri: URI = new URI(uri.getScheme, uri.getSchemeSpecificPart, null) // scalastyle:off null

    def xpointer: XPointer = XPointer.parse(uri.getFragment)
  }

  object AnyElemKey {

    def apply(docUri: URI, id: String): AnyElemKey = {
      AnyElemKey(new URI(docUri.getScheme, docUri.getSchemeSpecificPart, id))
    }

    def apply(docUri: URI, xpointer: XPointer): AnyElemKey = {
      AnyElemKey(new URI(docUri.getScheme, docUri.getSchemeSpecificPart, xpointer.toString))
    }
  }

  /**
   * Converts the given (referred) element in an XBRL taxonomy to a taxonomy element key. For an element declaration,
   * the element declaration key is chosen and not the concept key, since the substitution group is unknown from the XML
   * element name alone.
   */
  def fromReferredElement(elem: BackingNodes.Elem): TaxonomyElemKey = {
    import ENames._

    elem.name match {
      case XsElementEName => ElementKey(getTargetEName(elem))
      case LinkRoleTypeEName => RoleKey(elem.attr(RoleURIEName))
      case LinkArcroleTypeEName => ArcroleKey(elem.attr(ArcroleURIEName))
      case XsComplexTypeEName => TypeKey(getTargetEName(elem))
      case XsSimpleTypeEName => TypeKey(getTargetEName(elem))
      case _ =>
        val xpointer = XPointer.toXPointer(elem)
        val uri = new URI(elem.docUri.getScheme, elem.docUri.getSchemeSpecificPart, xpointer.toString)
        AnyElemKey(uri)
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
