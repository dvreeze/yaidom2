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

package eu.cdevreeze.yaidom2.queryapi.oo

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope

/**
 * So-called scoped element API. It extends trait `ClarkElemApi`, but it is also aware of qualified names (of elements
 * and attributes), and of in-scope namespaces. Hence the name `ScopedElemApi`. This API knows about XML elements,
 * but it does not know about nodes other than element nodes.
 *
 * @author Chris de Vreeze
 */
trait ScopedElemApi extends ClarkElemApi {

  type ThisElem <: ScopedElemApi

  def scope: Scope

  def qname: QName

  def attributesByQName: SeqMap[QName, String]

  def textAsQName: QName

  def textAsResolvedQName: EName

  def attrAsQNameOption(attributeName: EName): Option[QName]

  def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName]

  def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName]

  def attrAsQName(attributeName: EName): QName

  def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName

  def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName

  def attrAsResolvedQNameOption(attributeName: EName): Option[EName]

  def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName]

  def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName]

  def attrAsResolvedQName(attributeName: EName): EName

  def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName

  def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName
}

object ScopedElemApi {

  type Aux[E] = ScopedElemApi {
    type ThisElem = E
  }
}
