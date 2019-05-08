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

package eu.cdevreeze.yaidom2.queryapi.fun.internal

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.queryapi.fun.ScopedElemFunctionsApi

/**
 * Abstract partially implemented ScopedElemFunctionsApi, for re-usable (but overridable) partial element function implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not a stable as the purely abstract API.
 *
 * @author Chris de Vreeze
 */
trait AbstractScopedElemFunctions extends AbstractClarkElemFunctions with ScopedElemFunctionsApi {

  // ScopedElemFunctionsApi

  def textAsQName(elem: ElemType): QName = {
    QName.parse(text(elem).trim)
  }

  def textAsResolvedQName(elem: ElemType): EName = {
    scope(elem).resolveQNameOption(textAsQName(elem)).getOrElse(
      sys.error(s"Could not resolve QName-valued element text ${textAsQName(elem)}, given scope [${scope(elem)}]"))
  }

  def attrAsQNameOption(elem: ElemType, attributeName: EName): Option[QName] = {
    attrOption(elem, attributeName).map(v => QName.parse(v.trim))
  }

  def attrAsQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
    attrOption(elem, attributeNamespaceOption, attributeLocalName).map(v => QName.parse(v.trim))
  }

  def attrAsQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[QName] = {
    attrOption(elem, attributeNamespace, attributeLocalName).map(v => QName.parse(v.trim))
  }

  def attrAsQName(elem: ElemType, attributeName: EName): QName = {
    attrAsQNameOption(elem, attributeName).getOrElse(
      sys.error(s"Missing QName-valued attribute $attributeName"))
  }

  def attrAsQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
    attrAsQNameOption(elem, attributeNamespaceOption, attributeLocalName).getOrElse(
      sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
  }

  def attrAsQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): QName = {
    attrAsQNameOption(elem, attributeNamespace, attributeLocalName).getOrElse(
      sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
  }

  def attrAsResolvedQNameOption(elem: ElemType, attributeName: EName): Option[EName] = {
    attrAsQNameOption(elem, attributeName).map { qn =>
      scope(elem).resolveQNameOption(qn).getOrElse(
        sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope(elem)}]"))
    }
  }

  def attrAsResolvedQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
    attrAsQNameOption(elem, attributeNamespaceOption, attributeLocalName).map { qn =>
      scope(elem).resolveQNameOption(qn).getOrElse(
        sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope(elem)}]"))
    }
  }

  def attrAsResolvedQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[EName] = {
    attrAsQNameOption(elem, attributeNamespace, attributeLocalName).map { qn =>
      scope(elem).resolveQNameOption(qn).getOrElse(
        sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope(elem)}]"))
    }
  }

  def attrAsResolvedQName(elem: ElemType, attributeName: EName): EName = {
    attrAsResolvedQNameOption(elem, attributeName).getOrElse(
      sys.error(s"Missing QName-valued attribute $attributeName"))
  }

  def attrAsResolvedQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
    attrAsResolvedQNameOption(elem, attributeNamespaceOption, attributeLocalName).getOrElse(
      sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
  }

  def attrAsResolvedQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): EName = {
    attrAsResolvedQNameOption(elem, attributeNamespace, attributeLocalName).getOrElse(
      sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
  }
}

object AbstractScopedElemFunctions {

  type Aux[N, E] = AbstractScopedElemFunctions {
    type NodeType = N
    type ElemType = E
  }
}
