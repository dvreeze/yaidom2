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

/**
 * Clark element function API. See `ClarkElemApi`, but this is its non-OO functional counterpart. More precisely,
 * it is the non-OO functional counterpart of `ClarkNodes.Elem`.
 *
 * @author Chris de Vreeze
 */
trait ClarkElemFunctionsApi extends ElemFunctionsApi {

  type NodeType >: ElemType

  def name(elem: ElemType): EName

  def attributes(elem: ElemType): SeqMap[EName, String]

  /**
   * Returns the local name. That is, returns the local part of the name of the element.
   * This method must be fast in order to support fast local name queries.
   */
  def localName(elem: ElemType): String

  /**
   * Returns the optional namespace. That is, returns the optional namespace of the name of the element.
   * This method must be fast in order to support fast namespace queries.
   */
  def namespaceOption(elem: ElemType): Option[String]

  /**
   * Returns the equivalent of `namespaceOption.getOrElse("")`.
   * This method must be fast in order to support fast namespace queries.
   */
  def namespaceAsString(elem: ElemType): String

  def attrOption(elem: ElemType, attributeName: EName): Option[String]

  def attrOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String]

  def attrOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[String]

  /**
   * Finds an attribute that has no namespace by local name, if any.
   */
  def attrOption(elem: ElemType, attributeLocalName: String): Option[String]

  def attr(elem: ElemType, attributeName: EName): String

  def attr(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): String

  def attr(elem: ElemType, attributeNamespace: String, attributeLocalName: String): String

  /**
   * Gets an attribute that has no namespace by local name, throwing if no such attribute is found.
   */
  def attr(elem: ElemType, attributeLocalName: String): String

  def text(elem: ElemType): String

  def normalizedText(elem: ElemType): String

  def trimmedText(elem: ElemType): String

  /**
   * Returns all child nodes, of any kind of node (element node, text node etc.).
   */
  def children(elem: ElemType): Seq[NodeType]

  // No equivalent of the select method for Clark elements.
}

object ClarkElemFunctionsApi {

  type Aux[N, E] = ClarkElemFunctionsApi {
    type NodeType = N
    type ElemType = E
  }
}
