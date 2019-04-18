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

package eu.cdevreeze.yaidom2.queryapi.fun

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope

/**
 * Scoped element function API. See `ScopedElemApi`, but this is its non-OO functional counterpart. More precisely,
 * it is the non-OO functional counterpart of `ScopedNodes.Elem`.
 *
 * @author Chris de Vreeze
 */
trait ScopedElemFunctionsApi extends ClarkElemFunctionsApi {

  type ElemType

  def scope(elem: ElemType): Scope

  def qname(elem: ElemType): QName

  def attributesByQName(elem: ElemType): SeqMap[QName, String]

  def textAsQName(elem: ElemType): QName

  def textAsResolvedQName(elem: ElemType): EName

  def attrAsQNameOption(elem: ElemType, attributeName: EName): Option[QName]

  def attrAsQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName]

  def attrAsQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[QName]

  def attrAsQName(elem: ElemType, attributeName: EName): QName

  def attrAsQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): QName

  def attrAsQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): QName

  def attrAsResolvedQNameOption(elem: ElemType, attributeName: EName): Option[EName]

  def attrAsResolvedQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName]

  def attrAsResolvedQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[EName]

  def attrAsResolvedQName(elem: ElemType, attributeName: EName): EName

  def attrAsResolvedQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): EName

  def attrAsResolvedQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): EName
}

object ScopedElemFunctionsApi {

  type Aux[E, N] = ScopedElemFunctionsApi {
    type ElemType = E
    type NodeType = N
  }
}
