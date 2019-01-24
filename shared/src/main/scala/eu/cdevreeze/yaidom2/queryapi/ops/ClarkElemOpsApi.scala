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

package eu.cdevreeze.yaidom2.queryapi.ops

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ElemStep

/**
 * Clark element function API. See `ClarkElemApi`, but this is its non-OO functional counterpart. More precisely,
 * it is the non-OO functional counterpart of `ClarkNodes.Elem`.
 *
 * @author Chris de Vreeze
 */
trait ClarkElemOpsApi extends ElemOpsApi {

  type Elem

  type Node >: Elem

  def name(thisElem: Elem): EName

  def attributes(thisElem: Elem): Iterable[(EName, String)]

  /**
   * Returns the local name. That is, returns the local part of the name of the element.
   * This method must be fast in order to support fast local name queries.
   */
  def localName(thisElem: Elem): String

  /**
   * Returns the optional namespace. That is, returns the optional namespace of the name of the element.
   * This method must be fast in order to support fast namespace queries.
   */
  def namespaceOption(thisElem: Elem): Option[String]

  /**
   * Returns the equivalent of `namespaceOption.getOrElse("")`.
   * This method must be fast in order to support fast namespace queries.
   */
  def namespaceAsString(thisElem: Elem): String

  def attrOption(thisElem: Elem, attributeName: EName): Option[String]

  def attrOption(thisElem: Elem, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String]

  def attrOption(thisElem: Elem, attributeNamespace: String, attributeLocalName: String): Option[String]

  /**
   * Finds an attribute that has no namespace by local name, if any.
   */
  def attrOption(thisElem: Elem, attributeLocalName: String): Option[String]

  def attr(thisElem: Elem, attributeName: EName): String

  def attr(thisElem: Elem, attributeNamespaceOption: Option[String], attributeLocalName: String): String

  def attr(thisElem: Elem, attributeNamespace: String, attributeLocalName: String): String

  /**
   * Gets an attribute that has no namespace by local name, throwing if no such attribute is found.
   */
  def attr(thisElem: Elem, attributeLocalName: String): String

  def text(thisElem: Elem): String

  def normalizedText(thisElem: Elem): String

  def trimmedText(thisElem: Elem): String

  /**
   * Returns all child nodes, of any kind of node (element node, text node etc.).
   */
  def children(thisElem: Elem): Seq[Node]

  /**
   * Applies the given element step to this element.
   */
  def select(thisElem: Elem, step: ElemStep[Elem]): Seq[Elem]
}
