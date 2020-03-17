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

package eu.cdevreeze.yaidom2.creationapi

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.PrefixedScope

/**
 * Element creation API. Typical implementations are rather stable objects that use a NamespacePrefixMapper internally.
 *
 * It is not a type class, although it is easy to turn it into an imaginary type class with a type parameter for the element type.
 *
 * @author Chris de Vreeze
 */
trait ElemCreationApi {

  type NodeType

  type ElemType <: NodeType

  def emptyElem(name: EName, parentScope: PrefixedScope): ElemType

  def emptyElem(name: EName, attributes: SeqMap[EName, String], parentScope: PrefixedScope): ElemType

  def textElem(name: EName, txt: String, parentScope: PrefixedScope): ElemType

  def textElem(name: EName, attributes: SeqMap[EName, String], txt: String, parentScope: PrefixedScope): ElemType

  /**
   * Creates an element with the given name and children.
   *
   * Internally method usingParentScope is used to prevent prefixed namespace undeclarations, which are not allowed in XML 1.0.
   *
   * For performance it is best not to pass big child element trees.
   */
  def elem(name: EName, children: Seq[NodeType], parentScope: PrefixedScope): ElemType

  /**
   * Creates an element with the given name, attributes and children.
   *
   * Internally method usingParentScope is used to prevent prefixed namespace undeclarations, which are not allowed in XML 1.0.
   *
   * For performance it is best not to pass big child element trees.
   */
  def elem(name: EName, attributes: SeqMap[EName, String], children: Seq[NodeType], parentScope: PrefixedScope): ElemType

  /**
   * Returns all child nodes of the given element.
   */
  def children(elem: ElemType): Seq[NodeType]

  /**
   * Returns a copy of the given element in which the children have been replaced by the given collection of child nodes.
   *
   * Internally method usingParentScope is used to prevent prefixed namespace undeclarations, which are not allowed in XML 1.0.
   *
   * For performance it is best not to pass big child element trees.
   */
  def withChildren(elem: ElemType, newChildren: Seq[NodeType]): ElemType

  /**
   * Returns the equivalent of `withChildren(elem, children(elem).appended(child))`.
   *
   * For performance it is best not to pass big child element trees.
   */
  def plusChild(elem: ElemType, child: NodeType): ElemType

  /**
   * Returns the equivalent of `plusChildren(elem, childOption.toSeq)`.
   *
   * For performance it is best not to pass big child element trees.
   */
  def plusChildOption(elem: ElemType, childOption: Option[NodeType]): ElemType

  /**
   * Returns the equivalent of `withChildren(elem, children(elem).patch(index, Seq(child), 0))`.
   *
   * For performance it is best not to pass big child element trees.
   */
  def plusChild(elem: ElemType, index: Int, child: NodeType): ElemType

  /**
   * Returns the equivalent of `withChildren(elem, children(elem).patch(index, childOption.toSeq, 0))`.
   *
   * For performance it is best not to pass big child element trees.
   */
  def plusChildOption(elem: ElemType, index: Int, childOption: Option[NodeType]): ElemType

  /**
   * Returns the equivalent of `withChildren(elem, children(elem).appendedAll(childSeq))`.
   *
   * For performance it is best not to pass big child element trees.
   */
  def plusChildren(elem: ElemType, childSeq: Seq[NodeType]): ElemType

  /**
   * Returns `withChildren(elem, children(elem).patch(index, Seq.empty, 1))`. The in-scope namespaces of the element do not change.
   */
  def minusChild(elem: ElemType, index: Int): ElemType

  /**
   * Returns a copy of the given element in which the attributes have been replaced by the given collection of attributes.
   *
   * Internally method usingParentScope is used to prevent prefixed namespace undeclarations, which are not allowed in XML 1.0.
   */
  def withAttributes(elem: ElemType, newAttributes: SeqMap[EName, String]): ElemType

  /**
   * Returns a copy of the given element in which the given attribute has been added.
   */
  def plusAttribute(elem: ElemType, attrName: EName, attrValue: String): ElemType

  /**
   * Returns a copy of the given element in which the given optional attribute has been added.
   */
  def plusAttributeOption(elem: ElemType, attrName: EName, attrValueOption: Option[String]): ElemType

  /**
   * Returns a copy of the given element in which the given attributes have been added.
   */
  def plusAttributes(elem: ElemType, newAttributes: SeqMap[EName, String]): ElemType

  /**
   * Returns a copy of the given element in which the given attribute has been removed, if any.
   * The in-scope namespaces of the element do not change.
   */
  def minusAttribute(elem: ElemType, attrName: EName): ElemType

  /**
   * Recursively appends each element's scope to the parent scope, thus preventing the occurrence of prefixed namespace undeclarations
   * throughout the XML tree. After all, XML 1.0 does not allow any prefixed namespace undeclarations.
   *
   * This method is used internally in this element creation DSL, but is also handy in application code when adding
   * namespaces that are known to be used in attribute values or element text.
   */
  def usingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType
}

object ElemCreationApi {

  /**
   * This creation API type, restricting NodeType and ElemType to the passed type parameters.
   *
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[N, E] = ElemCreationApi {type NodeType = N; type ElemType = E}

  /**
   * OO API exposing most of the ElemCreationApi.
   */
  trait Elem {

    type UnderlyingNode >: UnderlyingElem

    type UnderlyingElem

    type ThisElem <: Elem

    def withChildren(newChildren: Seq[UnderlyingNode]): ThisElem

    def plusChild(child: UnderlyingNode): ThisElem

    def plusChildOption(childOption: Option[UnderlyingNode]): ThisElem

    def plusChild(index: Int, child: UnderlyingNode): ThisElem

    def plusChildOption(index: Int, childOption: Option[UnderlyingNode]): ThisElem

    def plusChildren(childSeq: Seq[UnderlyingNode]): ThisElem

    def minusChild(index: Int): ThisElem

    def withAttributes(newAttributes: SeqMap[EName, String]): ThisElem

    def plusAttribute(attrName: EName, attrValue: String): ThisElem

    def plusAttributeOption(attrName: EName, attrValueOption: Option[String]): ThisElem

    def plusAttributes(newAttributes: SeqMap[EName, String]): ThisElem

    def minusAttribute(attrName: EName): ThisElem

    def usingParentScope(parentScope: PrefixedScope): ThisElem

    def underlying: UnderlyingElem
  }

  object Elem {

    /**
     * This API type, restricting Node and Elem to the passed type parameters.
     *
     * @tparam N The underlying node type
     * @tparam E The underlying element type
     * @tparam W This (wrapper) element type
     */
    type Aux[N, E, W] = Elem {type UnderlyingNode = N; type UnderlyingElem = E; type ThisElem = W}
  }
}
