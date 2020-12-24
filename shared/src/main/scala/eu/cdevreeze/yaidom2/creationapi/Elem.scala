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
 * Element with an API to add/update children, attributes, QName, etc.
 *
 * @author Chris de Vreeze
 */
trait Elem {

  type ThisNode >: ThisElem

  type ThisElem <: Elem

  // Creation/update API

  def withChildren(newChildren: Seq[ThisNode]): ThisElem

  def plusChild(child: ThisNode): ThisElem

  def plusChildOption(childOption: Option[ThisNode]): ThisElem

  def plusChild(index: Int, child: ThisNode): ThisElem

  def plusChildOption(index: Int, childOption: Option[ThisNode]): ThisElem

  def plusChildren(childSeq: Seq[ThisNode]): ThisElem

  def minusChild(index: Int): ThisElem

  def withAttributes(newAttributes: ListMap[QName, String]): ThisElem

  def plusAttribute(attrQName: QName, attrValue: String): ThisElem

  def plusAttributeOption(attrQName: QName, attrValueOption: Option[String]): ThisElem

  def plusAttributes(newAttributes: ListMap[QName, String]): ThisElem

  def minusAttribute(attrQName: QName): ThisElem

  def withQName(newQName: QName): ThisElem

  // Note that we could add convenience methods combining calls to method
  // plusChildElem with a call to ElemCreationApi methods like emptyElem, textElem etc.
  // These convenience methods could be called plusEmptyChildElem, plusTextChildElem etc.
  // It was decided not to add those methods (which would also increase "type tangle"),
  // since their bang-for-the-buck is limited.

  // "Namespace declaration management"

  /**
   * Appends the given extra stable scope to this element's stable scope, and makes sure there are no namespace undeclarations
   * in the descendant elements. If the extra stable scope cannot be added as a non-conflicting scope to this element
   * or any of its descendants, an exception is thrown.
   *
   * This method is typically used to introduce one or more prefixes and corresponding namespaces to an element and
   * all its descendants. It can also be used to prevent the occurrence of namespace undeclarations, just by passing the
   * empty StableScope as extra scope.
   *
   * For element implementations that do not keep a (stable) scope, this is a no-op.
   */
  def usingExtraScopeDeeply(extraScope: StableScope): ThisElem

  /**
   * Appends the given extra stable scope to this element's stable scope, without affecting the scopes of descendant elements.
   * If the extra stable scope cannot be added as a non-conflicting scope to this element or any of its descendants, an exception is thrown.
   *
   * Note that this method typically leads to prefixed namespace undeclarations, which are not even allowed in XML 1.0.
   * This can be fixed by invoking method `usingExtraScopeDeeply` at some point.
   *
   * For element implementations that do not keep a (stable) scope, this is a no-op.
   */
  def withExtraScope(extraScope: StableScope): ThisElem

  /**
   * Returns `withExtraScope(StableScope.neededCompatibleSubScope(qnames, knownScope))`.
   *
   * For element implementations that do not keep a (stable) scope, this is a no-op.
   */
  def withNeededExtraScope(qnames: Set[QName], knownScope: StableScope): ThisElem
}

object Elem {

  /**
   * This API type, restricting Node and Elem to the passed type parameters.
   *
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[N, E] = Elem { type ThisNode = N; type ThisElem = E }
}
