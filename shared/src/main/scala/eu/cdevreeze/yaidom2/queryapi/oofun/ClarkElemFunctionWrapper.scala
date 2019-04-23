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
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.fun.ClarkElemFunctionsApi
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkNodes

/**
 * ClarkElemFunctionApi wrapper around ClarkElemNodes.Elem.
 *
 * @author Chris de Vreeze
 */
trait ClarkElemFunctionWrapper extends ClarkElemFunctionsApi {

  type ElemType <: ClarkNodes.Elem.Aux[NodeType, ElemType]

  type NodeType >: ElemType <: ClarkNodes.Node

  final def filterChildElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    elem.filterChildElems(p)
  }

  final def findAllChildElems(elem: ElemType): Seq[ElemType] = {
    elem.findAllChildElems()
  }

  final def findChildElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    elem.findChildElem(p)
  }

  final def filterDescendantElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    elem.filterDescendantElems(p)
  }

  final def findAllDescendantElems(elem: ElemType): Seq[ElemType] = {
    elem.findAllDescendantElems()
  }

  final def findDescendantElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    elem.findDescendantElem(p)
  }

  final def filterDescendantElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    elem.filterDescendantElemsOrSelf(p)
  }

  final def findAllDescendantElemsOrSelf(elem: ElemType): Seq[ElemType] = {
    elem.findAllDescendantElemsOrSelf()
  }

  final def findDescendantElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    elem.findDescendantElemOrSelf(p)
  }

  final def findTopmostElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    elem.findTopmostElems(p)
  }

  final def findTopmostElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    elem.findTopmostElemsOrSelf(p)
  }

  final def name(elem: ElemType): EName = {
    elem.name
  }

  final def attributes(elem: ElemType): SeqMap[EName, String] = {
    elem.attributes
  }

  final def localName(elem: ElemType): String = {
    elem.localName
  }

  final def namespaceOption(elem: ElemType): Option[String] = {
    elem.namespaceOption
  }

  final def namespaceAsString(elem: ElemType): String = {
    elem.namespaceAsString
  }

  final def attrOption(elem: ElemType, attributeName: EName): Option[String] = {
    elem.attrOption(attributeName)
  }

  final def attrOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
    elem.attrOption(attributeNamespaceOption, attributeLocalName)
  }

  final def attrOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[String] = {
    elem.attrOption(attributeNamespace, attributeLocalName)
  }

  final def attrOption(elem: ElemType, attributeLocalName: String): Option[String] = {
    elem.attrOption(attributeLocalName)
  }

  final def attr(elem: ElemType, attributeName: EName): String = {
    elem.attr(attributeName)
  }

  final def attr(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
    elem.attr(attributeNamespaceOption, attributeLocalName)
  }

  final def attr(elem: ElemType, attributeNamespace: String, attributeLocalName: String): String = {
    elem.attr(attributeNamespace, attributeLocalName)
  }

  final def attr(elem: ElemType, attributeLocalName: String): String = {
    elem.attr(attributeLocalName)
  }

  final def text(elem: ElemType): String = {
    elem.text
  }

  final def normalizedText(elem: ElemType): String = {
    elem.normalizedText
  }

  final def trimmedText(elem: ElemType): String = {
    elem.trimmedText
  }

  final def children(elem: ElemType): Seq[NodeType] = {
    elem.children
  }

  final def select(elem: ElemType, step: ElemStep[ElemType]): Seq[ElemType] = {
    elem.select(step)
  }
}
