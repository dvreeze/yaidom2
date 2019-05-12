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

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.ENames

/**
 * Type of element as far as a "locator" is concerned.
 */
sealed trait TaxonomyElemKey {
  def kind: String
}

sealed trait NamedSchemaElementKey extends TaxonomyElemKey {
  def name: EName
}

object TaxonomyElemKey {

  final case class ElementDeclaration(name: EName) extends NamedSchemaElementKey {
    override def toString: String = name.toString
    def kind: String = "elementDecl"
  }

  final case class RoleType(uri: String) extends TaxonomyElemKey {
    override def toString: String = uri
    def kind: String = "roleType"
  }

  final case class ArcroleType(uri: String) extends TaxonomyElemKey {
    override def toString: String = uri
    def kind: String = "arcroleType"
  }

  final case class NamedType(name: EName) extends NamedSchemaElementKey {
    override def toString: String = name.toString
    def kind: String = "namedType"
  }

  final case class EnumerationValue(id: String) extends TaxonomyElemKey {
    override def toString: String = id
    def kind: String = "enumerationValue"
  }

  final case class Id(id: String) extends TaxonomyElemKey {
    override def toString: String = id
    def kind: String = "id"
  }

  def from(elem: BackingNodes.Elem): TaxonomyElemKey = {
    import ENames._

    elem.name match {
      case XsElementEName => ElementDeclaration(getTargetEName(elem))
      case LinkRoleTypeEName => RoleType(elem.attr(RoleURIEName))
      case LinkArcroleTypeEName => RoleType(elem.attr(ArcroleURIEName))
      case XsComplexTypeEName => NamedType(getTargetEName(elem))
      case XsSimpleTypeEName => NamedType(getTargetEName(elem))
      case XsEnumerationEName =>
        require(elem.attrOption(IdEName).nonEmpty, s"Missing ID on ${elem.name}")
        EnumerationValue(elem.attr(IdEName))
      case _ =>
        require(elem.attrOption(IdEName).nonEmpty, s"Missing ID on ${elem.name}")
        Id(elem.attr(IdEName))
    }
  }

  def getTargetEName(elem: BackingNodes.Elem): EName = {
    require(elem.attrOption(ENames.NameEName).nonEmpty, s"Missing name attribute on ${elem.name}")
    val tnsOption: Option[String] =
      elem.findAncestorElemOrSelf(_.name == ENames.XsSchemaEName).flatMap(_.attrOption(ENames.TargetNamespaceEName))
    EName(tnsOption, elem.attr(ENames.NameEName))
  }
}
