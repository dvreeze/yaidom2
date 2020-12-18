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

import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.StableScope

import scala.collection.immutable.ListMap

/**
 * Element within a known stable scope. It helps in keeping (gradually growing) known stable scopes
 * around while creating element trees.
 *
 * If the element has a Scope, it must be a StableScope, and it must be a compatible
 * sub-scope of the known stable scope.
 *
 * All methods that add/update child nodes, attributes and that update element QNames must return element wrappers
 * whose known scopes are compatible super-scopes of this wrapper's known scope.
 *
 * @author Chris de Vreeze
 */
trait ElemInKnownScope {

  /**
   * The element wrapper type, holding an element and a known scope.
   */
  type WrapperType <: ElemInKnownScope

  type NodeType

  type ElemType <: NodeType

  // The state carried by this object

  def elem: ElemType

  def knownStableScope: StableScope

  // Creation/update API

  def withChildren(newChildren: Seq[NodeType]): WrapperType

  def plusChild(child: NodeType): WrapperType

  def plusChildOption(childOption: Option[NodeType]): WrapperType

  def plusChild(index: Int, child: NodeType): WrapperType

  def plusChildOption(index: Int, childOption: Option[NodeType]): WrapperType

  def plusChildren(childSeq: Seq[NodeType]): WrapperType

  def minusChild(index: Int): WrapperType

  def withAttributes(newAttributes: ListMap[QName, String]): WrapperType

  def plusAttribute(attrQName: QName, attrValue: String): WrapperType

  def plusAttributeOption(attrQName: QName, attrValueOption: Option[String]): WrapperType

  def plusAttributes(newAttributes: ListMap[QName, String]): WrapperType

  def minusAttribute(attrQName: QName): WrapperType

  def withQName(newQName: QName): WrapperType

  /**
   * Returns `plusChild(childElem.elem)`, ignoring the child's known scope.
   */
  def plusChildElem(childElem: WrapperType): WrapperType

  /**
   * Returns `plusChildOption(childElemOption.map(_.elem))`, ignoring the optional child's known scope.
   */
  def plusChildElemOption(childElemOption: Option[WrapperType]): WrapperType

  /**
   * Returns `plusChild(index, childElem.elem)`, ignoring the child's known scope.
   */
  def plusChildElem(index: Int, childElem: WrapperType): WrapperType

  /**
   * Returns `plusChildOption(index, childElemOption.map(_.elem))`, ignoring the optional child's known scope.
   */
  def plusChildElemOption(index: Int, childElemOption: Option[WrapperType]): WrapperType

  /**
   * Returns `plusChildren(childElemSeq.map(_.elem))`, ignoring the known scopes of the passed children.
   */
  def plusChildElems(childElemSeq: Seq[WrapperType]): WrapperType

  /**
   * Appends the parent stable scope to this element's stable scope, and makes sure there are no namespace undeclarations
   * in the descendant elements. If the parent stable scope cannot be added as a non-conflicting scope to this element
   * or any of its descendants, an exception is thrown.
   *
   * This method is typically used to introduce one or more prefixes and corresponding namespaces to an element and
   * all its descendants.
   */
  def usingParentScope(parentScope: StableScope): WrapperType
}

object ElemInKnownScope {

  /**
   * This API type, restricting Wrapper, Node and Elem to the passed type parameters.
   *
   * @tparam W The wrapper self type
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[W, N, E] = ElemInKnownScope { type WrapperType = W; type NodeType = N; type ElemType = E }
}
