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
 * For element creation functions that take an extra `neededExtraStableScope` stable scope parameter, expression
 * `knownStableScope.appendNonConflictingScope(neededExtraStableScope)` must not throw an exception.
 *
 * @author Chris de Vreeze
 */
trait ElemCreationApi {

  type NodeType

  type ElemType <: NodeType

  def knownStableScope: StableScope

  /**
   * Creates an empty element with the given QName.
   */
  def emptyElem(qname: QName): ElemType

  /**
   * Creates an empty element with the given QName.
   */
  def emptyElem(qname: QName, neededExtraStableScope: StableScope): ElemType

  /**
   * Creates an empty element with the given QName and given attributes.
   */
  def emptyElem(qname: QName, attributesByQName: ListMap[QName, String]): ElemType

  /**
   * Creates an empty element with the given QName and given attributes.
   */
  def emptyElem(qname: QName, attributesByQName: ListMap[QName, String], neededExtraStableScope: StableScope): ElemType

  /**
   * Creates an element with the given QName and single text child node with the given text content.
   */
  def textElem(qname: QName, txt: String): ElemType

  /**
   * Creates an element with the given QName and single text child node with the given text content.
   */
  def textElem(qname: QName, txt: String, neededExtraStableScope: StableScope): ElemType

  /**
   * Creates an element with the given QName, attributes and single text child node with the given text content.
   */
  def textElem(qname: QName, attributesByQName: ListMap[QName, String], txt: String): ElemType

  /**
   * Creates an element with the given QName, attributes and single text child node with the given text content.
   */
  def textElem(qname: QName, attributesByQName: ListMap[QName, String], txt: String, neededExtraStableScope: StableScope): ElemType

  /**
   * Creates an element with the given name and children.
   *
   * This may be an expensive operation (because of the number or depth of the children).
   */
  def elem(qname: QName, children: Seq[NodeType]): ElemType

  /**
   * Creates an element with the given name and children.
   *
   * This may be an expensive operation (because of the number or depth of the children).
   */
  def elem(qname: QName, children: Seq[NodeType], neededExtraStableScope: StableScope): ElemType

  /**
   * Creates an element with the given name, attributes and children.
   *
   * This may be an expensive operation (because of the number or depth of the children).
   */
  def elem(qname: QName, attributesByQName: ListMap[QName, String], children: Seq[NodeType]): ElemType

  /**
   * Creates an element with the given name, attributes and children.
   *
   * This may be an expensive operation (because of the number or depth of the children).
   */
  def elem(qname: QName, attributesByQName: ListMap[QName, String], children: Seq[NodeType], neededExtraStableScope: StableScope): ElemType
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
      elemQName.prefixOption.filterNot(_ == "xml").forall(pref => stableScope.keySet.contains(pref)),
      s"Can not resolve element QName $elemQName in scope ${stableScope.scope}"
    )

    attributeQNames.flatMap(_.prefixOption).filterNot(_ == "xml").foreach { pref =>
      require(
        stableScope.keySet.contains(pref.ensuring(_.nonEmpty)),
        s"Can not resolve attribute QName prefix $pref in scope ${stableScope.scope}")
    }

    val prefixes: Set[String] = elemQName.prefixOption.toSet.union(attributeQNames.flatMap(_.prefixOption))

    stableScope.filterKeysCompatibly(prefixes)
  }

  /**
   * This API type, restricting Node and Elem to the passed type parameters.
   *
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[N, E] = ElemCreationApi { type NodeType = N; type ElemType = E }
}
