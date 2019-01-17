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

package eu.cdevreeze.yaidom2.queryapi

import eu.cdevreeze.yaidom2.core.EName

/**
 * Clark element API. It extends trait `ElemApi`, and offers methods to query for element text and element attributes.
 * Therefore this API knows about XML elements, but it does not know about nodes other than element nodes.
 * Clark elements know about expanded names of elements and attributes, but not about qualified names, nor
 * namespace scopes.
 *
 * @author Chris de Vreeze
 */
trait ClarkElemApi extends Any with ElemApi {

  type ThisElem <: ClarkElemApi

  def name: EName

  def attributes: Iterable[(EName, String)]

  def localName: String

  def findAttribute(attributeName: EName): Option[String]

  def getAttribute(attributeName: EName): String

  def findAttributeByLocalName(attributeLocalName: String): Option[String]

  def text: String

  def normalizedText: String

  def trimmedText: String
}

object ClarkElemApi {

  type Aux[E] = ClarkElemApi {
    type ThisElem = E
  }
}
