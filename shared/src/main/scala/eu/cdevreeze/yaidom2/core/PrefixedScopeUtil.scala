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

package eu.cdevreeze.yaidom2.core

import scala.collection.immutable.ListMap

/**
 * Utility to manipulate PrefixedScope instances, using a NamespacePrefixMapper.
 *
 * @author Chris de Vreeze
 */
final class PrefixedScopeUtil(val namespacePrefixMapper: NamespacePrefixMapper) {

  /**
   * Calls the extractScope method taking a collection of ENames and a parent PrefixedScope.
   */
  def extractScope(ename: EName, parentScope: PrefixedScope): PrefixedScope = {
    extractScope(Seq(ename), parentScope)
  }

  /**
   * Calls `extractScopeFromNamespaces(enames.flatMap(_.namespaceUriOption).distinct, parentScope)`.
   */
  def extractScope(enames: Seq[EName], parentScope: PrefixedScope): PrefixedScope = {
    val namespaces: Seq[String] = enames.flatMap(_.namespaceUriOption).distinct

    extractScopeFromNamespaces(namespaces, parentScope)
  }

  /**
   * Calls the extractScopeFromNamespaces method taking a collection of namespaces and a parent PrefixedScope.
   */
  def extractScopeFromNamespace(ns: String, parentScope: PrefixedScope): PrefixedScope = {
    extractScopeFromNamespaces(Seq(ns), parentScope)
  }

  /**
   * Extracts a PrefixedScope from the given namespaces, prepending it with the given parent PrefixedScope.
   * For each of the given namespaces, the corresponding prefix is found, first by checking the parent
   * PrefixedScope (through method PrefixedScope.findPrefixForNamespace), and if not found, then by using the NamespacePrefixMapper.
   *
   * This function throws an exception if neither parent PrefixedScope nor the NamespaceMapper have a prefix for some namespace.
   * It also throws an exception if the resulting PrefixedScope would conflict with the given parent PrefixedScope,
   * due to a changed prefix-namespace mapping.
   *
   * It is guaranteed that the resulting PrefixedScope is a super-scope of the passed parent PrefixedScope.
   *
   * Note that this method is also very suitable for filling a PrefixedScope from the NamespaceMapper, by passing an empty
   * PrefixedScope as 2nd parameter.
   */
  def extractScopeFromNamespaces(namespaces: Seq[String], parentScope: PrefixedScope): PrefixedScope = {
    val prefixNamespaceMap: ListMap[String, String] = namespaces
      .map { ns =>
        val prefix = parentScope.findPrefixForNamespace(ns).getOrElse(namespacePrefixMapper.getPrefix(ns)) // Throws if no prefix found!
        prefix -> ns
      }
      .to(ListMap)

    val resultScope = parentScope.append(PrefixedScope.from(prefixNamespaceMap))
    require(parentScope.doesNotConflictWith(resultScope), s"Conflicting scopes '$parentScope' and '$resultScope'")
    resultScope.ensuring(_.superScopeOf(parentScope))
  }
}
