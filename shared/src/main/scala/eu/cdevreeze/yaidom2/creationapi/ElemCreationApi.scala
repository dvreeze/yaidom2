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
 * Element creation API, using stable scopes underneath.
 *
 * All element creation functions return an `ElemInKnownScope`, which combines an element with a known stable scope.
 *
 * The element creation functions below may or may not have a parameter for the needed
 * extra stable scope. If they do, expression `knownStableScope.canAppendUnsafely(neededExtraStableScope)`
 * must return true.
 *
 * Thus, for the element creation functions below, the "context" stable scope is
 * `knownStableScope.appendUnsafely(neededExtraStableScope)` if `neededExtraStableScope` is passed
 * as extra parameter, and `knownStableScope` otherwise. The stable scope of the resulting created element
 * is the result of calling function `ElementCreationApi.minimizeStableScope`, passing the above-mentioned
 * "context" stable scope as first parameter, and the element QName and attribute QNames as other parameters.
 * An exception will be thrown if the element QName or attribute QNames cannot be resolved. The `neededExtraStableScope`
 * itself is also added to the element creation function result's scope.
 *
 * Hence, the stable scope of the element creation function result's element is a sub-scope of
 * `knownStableScope.appendUnsafely(neededExtraStableScope)`, but a super-scope of `neededExtraStableScope` (where
 * `neededExtraStableScope` defaults to the empty stable scope). The function result also contains the default namespace
 * if `knownStableScope` does, both in the returned element and the returned adapted known scope.
 *
 * If the element creation function also takes child nodes, the child element trees must have stable scopes that can be
 * appended to above-mentioned "context" stable scope, or else an exception is thrown. These element creation functions
 * make sure that prefixed namespace undeclarations do not occur, which means that the scopes of descendant elements
 * may be enhanced by the function implementation.
 *
 * @author Chris de Vreeze
 */
trait ElemCreationApi {

  type WrapperType <: ElemInKnownScope

  type NodeType

  type ElemType <: NodeType

  def knownStableScope: StableScope

  /**
   * Creates an empty element with the given QName.
   * See the trait level API documentation for the semantics.
   */
  def emptyElem(qname: QName): WrapperType

  /**
   * Creates an empty element with the given QName.
   * See the trait level API documentation for the semantics.
   */
  def emptyElem(qname: QName, neededExtraStableScope: StableScope): WrapperType

  /**
   * Creates an empty element with the given QName and given attributes.
   * See the trait level API documentation for the semantics.
   */
  def emptyElem(qname: QName, attributesByQName: ListMap[QName, String]): WrapperType

  /**
   * Creates an empty element with the given QName and given attributes.
   * See the trait level API documentation for the semantics.
   */
  def emptyElem(qname: QName, attributesByQName: ListMap[QName, String], neededExtraStableScope: StableScope): WrapperType

  /**
   * Creates an element with the given QName and single text child node with the given text content.
   * See the trait level API documentation for the semantics.
   */
  def textElem(qname: QName, txt: String): WrapperType

  /**
   * Creates an element with the given QName and single text child node with the given text content.
   * See the trait level API documentation for the semantics.
   */
  def textElem(qname: QName, txt: String, neededExtraStableScope: StableScope): WrapperType

  /**
   * Creates an element with the given QName, attributes and single text child node with the given text content.
   * See the trait level API documentation for the semantics.
   */
  def textElem(qname: QName, attributesByQName: ListMap[QName, String], txt: String): WrapperType

  /**
   * Creates an element with the given QName, attributes and single text child node with the given text content.
   * See the trait level API documentation for the semantics.
   */
  def textElem(qname: QName, attributesByQName: ListMap[QName, String], txt: String, neededExtraStableScope: StableScope): WrapperType

  /**
   * Creates an element with the given name and children.
   * See the trait level API documentation for the semantics.
   *
   * The resulting element tree's stable scopes have all been appended to the "known stable scope".
   *
   * This may be an expensive operation (because of the number or depth of the children).
   */
  def elem(qname: QName, children: Seq[NodeType]): WrapperType

  /**
   * Creates an element with the given name and children.
   * See the trait level API documentation for the semantics.
   *
   * The resulting element tree's stable scopes have all been appended to the "known stable scope".
   *
   * This may be an expensive operation (because of the number or depth of the children).
   */
  def elem(qname: QName, children: Seq[NodeType], neededExtraStableScope: StableScope): WrapperType

  /**
   * Creates an element with the given name, attributes and children.
   * See the trait level API documentation for the semantics.
   *
   * The resulting element tree's stable scopes have all been appended to the "known stable scope".
   *
   * This may be an expensive operation (because of the number or depth of the children).
   */
  def elem(qname: QName, attributesByQName: ListMap[QName, String], children: Seq[NodeType]): WrapperType

  /**
   * Creates an element with the given name, attributes and children.
   * See the trait level API documentation for the semantics.
   *
   * The resulting element tree's stable scopes have all been appended to the "known stable scope".
   *
   * This may be an expensive operation (because of the number or depth of the children).
   */
  def elem(qname: QName, attributesByQName: ListMap[QName, String], children: Seq[NodeType], neededExtraStableScope: StableScope): WrapperType
}

object ElemCreationApi {

  /**
   * Returns the minimal sub-scope of the parameter stable scope that can resolve the given element QName and attribute names.
   * If element or attribute QNames can not be resolved, an exception is thrown. If the parameter stable scope has a default
   * namespace, the result stable scope has it as well, regardless whether it is used. That is, the result stable scope
   * is a compatible sub-scope of the parameter stable scope.
   */
  def minimizeStableScope(stableScope: StableScope, elemQName: QName, attributeQNames: Set[QName]): StableScope = {
    require(
      elemQName.prefixOption.forall(pref => stableScope.keySet.contains(pref)),
      s"Can not resolve element QName $elemQName in scope ${stableScope.scope}")

    attributeQNames.flatMap(_.prefixOption).foreach { pref =>
      require(
        stableScope.keySet.contains(pref.ensuring(_.nonEmpty)),
        s"Can not resolve attribute QName prefix $pref in scope ${stableScope.scope}")
    }

    val prefixes: Set[String] = elemQName.prefixOption.toSet.union(attributeQNames.flatMap(_.prefixOption))

    val result: StableScope = stableScope.filterKeys(prefixes + "") // The default namespace, if any, is retained
    result.ensuring(_.isCompatibleSubScopeOf(stableScope))
  }

  /**
   * This API type, restricting Wrapper, Node and Elem to the passed type parameters.
   *
   * @tparam W The wrapper self type
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[W, N, E] = ElemCreationApi { type WrapperType = W; type NodeType = N; type ElemType = E }
}
