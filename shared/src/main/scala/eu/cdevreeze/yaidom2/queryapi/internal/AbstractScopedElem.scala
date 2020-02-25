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

package eu.cdevreeze.yaidom2.queryapi.internal

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes

/**
 * Abstract partially implemented ScopedNodes.Elem, for re-usable (but overridable) partial element implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not as stable as the purely abstract API.
 *
 * In concrete element classes extending this trait (directly or indirectly), strongly consider overriding all methods
 * that contain type member ThisElem anywhere in the method signature, by just calling the super-trait version of the method.
 * That would ensure that in those method signatures type member ThisElem has the correct concrete element type.
 *
 * @author Chris de Vreeze
 */
trait AbstractScopedElem extends AbstractClarkElem with ScopedNodes.Elem {

  type ThisElem <: AbstractScopedElem.Aux[ThisNode, ThisElem]

  // ScopedElemApi

  def textAsQName: QName = {
    QName.parse(text.trim)
  }

  def textAsResolvedQName: EName = {
    scope.resolveQNameOption(textAsQName).getOrElse(
      sys.error(s"Could not resolve QName-valued element text $textAsQName, given scope [$scope]"))
  }

  def attrAsQNameOption(attributeName: EName): Option[QName] = {
    attrOption(attributeName).map(v => QName.parse(v.trim))
  }

  def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
    attrOption(attributeNamespaceOption, attributeLocalName).map(v => QName.parse(v.trim))
  }

  def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
    attrOption(attributeNamespace, attributeLocalName).map(v => QName.parse(v.trim))
  }

  def attrAsQName(attributeName: EName): QName = {
    attrAsQNameOption(attributeName).getOrElse(
      sys.error(s"Missing QName-valued attribute $attributeName"))
  }

  def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
    attrAsQNameOption(attributeNamespaceOption, attributeLocalName).getOrElse(
      sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
  }

  def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
    attrAsQNameOption(attributeNamespace, attributeLocalName).getOrElse(
      sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
  }

  def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
    attrAsQNameOption(attributeName).map { qn =>
      scope.resolveQNameOption(qn).getOrElse(
        sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [$scope]"))
    }
  }

  def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
    attrAsQNameOption(attributeNamespaceOption, attributeLocalName).map { qn =>
      scope.resolveQNameOption(qn).getOrElse(
        sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [$scope]"))
    }
  }

  def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
    attrAsQNameOption(attributeNamespace, attributeLocalName).map { qn =>
      scope.resolveQNameOption(qn).getOrElse(
        sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [$scope]"))
    }
  }

  def attrAsResolvedQName(attributeName: EName): EName = {
    attrAsResolvedQNameOption(attributeName).getOrElse(
      sys.error(s"Missing QName-valued attribute $attributeName"))
  }

  def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
    attrAsResolvedQNameOption(attributeNamespaceOption, attributeLocalName).getOrElse(
      sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
  }

  def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
    attrAsResolvedQNameOption(attributeNamespace, attributeLocalName).getOrElse(
      sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
  }
}

object AbstractScopedElem {

  type Aux[N, E] = AbstractScopedElem {type ThisNode = N; type ThisElem = E}
}
