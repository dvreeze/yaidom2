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

package eu.cdevreeze.yaidom2.queryapi.oofun

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.fun.ScopedElemFunctionsApi
import eu.cdevreeze.yaidom2.queryapi.oo.ScopedNodes

/**
 * ScopedElemFunctionApi wrapper around ScopedElemNodes.Elem.
 *
 * @author Chris de Vreeze
 */
trait ScopedElemFunctionWrapper extends ClarkElemFunctionWrapper with ScopedElemFunctionsApi {

  type ElemType <: ScopedNodes.Elem.Aux[NodeType, ElemType]

  type NodeType >: ElemType <: ScopedNodes.Node

  final def scope(elem: ElemType): Scope = {
    elem.scope
  }

  final def qname(elem: ElemType): QName = {
    elem.qname
  }

  final def attributesByQName(elem: ElemType): SeqMap[QName, String] = {
    elem.attributesByQName
  }

  final def textAsQName(elem: ElemType): QName = {
    elem.textAsQName
  }

  final def textAsResolvedQName(elem: ElemType): EName = {
    elem.textAsResolvedQName
  }

  final def attrAsQNameOption(elem: ElemType, attributeName: EName): Option[QName] = {
    elem.attrAsQNameOption(attributeName)
  }

  final def attrAsQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
    elem.attrAsQNameOption(attributeNamespaceOption, attributeLocalName)
  }

  final def attrAsQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[QName] = {
    elem.attrAsQNameOption(attributeNamespace, attributeLocalName)
  }

  final def attrAsQName(elem: ElemType, attributeName: EName): QName = {
    elem.attrAsQName(attributeName)
  }

  final def attrAsQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
    elem.attrAsQName(attributeNamespaceOption, attributeLocalName)
  }

  final def attrAsQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): QName = {
    elem.attrAsQName(attributeNamespace, attributeLocalName)
  }

  final def attrAsResolvedQNameOption(elem: ElemType, attributeName: EName): Option[EName] = {
    elem.attrAsResolvedQNameOption(attributeName)
  }

  final def attrAsResolvedQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
    elem.attrAsResolvedQNameOption(attributeNamespaceOption, attributeLocalName)
  }

  final def attrAsResolvedQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[EName] = {
    elem.attrAsResolvedQNameOption(attributeNamespace, attributeLocalName)
  }

  final def attrAsResolvedQName(elem: ElemType, attributeName: EName): EName = {
    elem.attrAsResolvedQName(attributeName)
  }

  final def attrAsResolvedQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
    elem.attrAsResolvedQName(attributeNamespaceOption, attributeLocalName)
  }

  final def attrAsResolvedQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): EName = {
    elem.attrAsResolvedQName(attributeNamespace, attributeLocalName)
  }
}
