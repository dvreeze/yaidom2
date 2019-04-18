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

/**
 * Clark element API. It extends trait `ElemApi`, and offers methods to query for element text and element attributes.
 * Therefore this API knows about XML elements, but it does not know about nodes other than element nodes.
 * Clark elements know about expanded names of elements and attributes, but not about qualified names, nor
 * namespace scopes.
 *
 * This query API trait has been named after James Clark, who came up with a minimal XML abstraction in which
 * only expanded names (for elements and attributes) occur, and no qualified names.
 *
 * @author Chris de Vreeze
 */
trait ClarkElemApi extends ElemApi {

  type ThisElem <: ClarkElemApi

  def name: EName

  def attributes: SeqMap[EName, String]

  /**
   * Returns the local name. That is, returns the local part of the name of the element.
   * This method must be fast in order to support fast local name queries.
   */
  def localName: String

  /**
   * Returns the optional namespace. That is, returns the optional namespace of the name of the element.
   * This method must be fast in order to support fast namespace queries.
   */
  def namespaceOption: Option[String]

  /**
   * Returns the equivalent of `namespaceOption.getOrElse("")`.
   * This method must be fast in order to support fast namespace queries.
   */
  def namespaceAsString: String

  def attrOption(attributeName: EName): Option[String]

  def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String]

  def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String]

  /**
   * Finds an attribute that has no namespace by local name, if any.
   */
  def attrOption(attributeLocalName: String): Option[String]

  def attr(attributeName: EName): String

  def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String

  def attr(attributeNamespace: String, attributeLocalName: String): String

  /**
   * Gets an attribute that has no namespace by local name, throwing if no such attribute is found.
   */
  def attr(attributeLocalName: String): String

  def text: String

  def normalizedText: String

  def trimmedText: String
}

object ClarkElemApi {

  type Aux[E] = ClarkElemApi {
    type ThisElem = E
  }
}
