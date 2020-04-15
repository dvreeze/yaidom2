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
 * A holder of a Scope that has no default namespace. It is useful for element creation DSLs, for example.
 *
 * Prefixed scopes are more predictable than scopes in general. By not containing any default namespace
 * unprefixed QNames always have no namespace, and cannot suddenly and silently get a namespace by the appearance of a
 * default namespace anywhere in the element ancestry. Hence, prefixed scopes are preferable to scopes in general when used
 * in element creation DSLs.
 *
 * Prefixed scopes are not necessarily invertible, so there may be more than one prefix for any given namespace.
 *
 * @author Chris de Vreeze
 */
final case class PrefixedScope private (scope: Scope) {
  assert(scope.defaultNamespaceOption.isEmpty)

  override def toString: String = scope.toString

  /** Returns true if this is a subscope of the given parameter `PrefixedScope`. A `PrefixedScope` is considered subscope of itself. */
  def subScopeOf(otherPrefixedScope: PrefixedScope): Boolean = {
    scope.subScopeOf(otherPrefixedScope.scope)
  }

  /** Returns true if this is a superscope of the given parameter `PrefixedScope`. A `PrefixedScope` is considered superscope of itself. */
  def superScopeOf(otherPrefixedScope: PrefixedScope): Boolean = otherPrefixedScope.subScopeOf(this)

  /** Returns `PrefixedScope.from(scope.filter(p))`. */
  def filter(p: ((String, String)) => Boolean): PrefixedScope = PrefixedScope.from(scope.filter(p))

  /** Returns `PrefixedScope.from(scope.filterKeys(p))`. */
  def filterKeys(p: String => Boolean): PrefixedScope = {
    PrefixedScope.from(scope.filterKeys(p))
  }

  /** Returns `scope.keySet`. */
  def keySet: Set[String] = scope.keySet

  /** Returns `scope.namespaces`. Hence, the "XML namespace" is not returned. */
  def namespaces: Set[String] = scope.namespaces

  /** Returns `PrefixedScope.from(scope.filterNamespaces(p))`. */
  def filterNamespaces(p: String => Boolean): PrefixedScope = {
    PrefixedScope.from(scope.filterNamespaces(p))
  }

  /**
   * Returns `scope.resolveQName(qname)`.
   */
  def resolveQName(qname: QName): EName = {
    scope.resolveQName(qname)
  }

  /**
   * Returns `scope.resolveQNameOption(qname)`.
   */
  def resolveQNameOption(qname: QName): Option[EName] = {
    scope.resolveQNameOption(qname)
  }

  /**
   * Finds an optional QName for the given EName. See method findPrefixForNamespace for the choice of prefix, if any.
   */
  def findQName(ename: EName): Option[QName] = {
    if (ename.namespaceUriOption.isEmpty) {
      Some(QName.fromLocalName(ename.localPart))
    } else {
      if (ename.namespaceUriOption.contains(Scope.XmlNamespace)) {
        Some(QName("xml", ename.localPart))
      } else {
        val prefixes: Seq[String] = scope.prefixesForNamespace(ename.namespaceUriOption.get).ensuring(_.forall(_.nonEmpty))

        val prefixOption: Option[String] = prefixes.lastOption

        prefixOption.map { prefix =>
          QName(prefix, ename.localPart)
        }
      }
    }.ensuring(_.forall(qn => scope.resolveQNameOption(qn).contains(ename)))
  }

  /**
   * Returns the equivalent of `findQName(ename).get`.
   */
  def getQName(ename: EName): QName = {
    findQName(ename).getOrElse(sys.error(s"No QName found for EName '$ename' using prefixed scope $this"))
  }

  /**
   * Finds an optional prefix for the given namespace. The last added prefix (in prefix-namespace mapping insertion order), if any, is returned.
   * For built-in namespace "http://www.w3.org/XML/1998/namespace", prefix "xml" is returned, wrapped in an Option.
   */
  def findPrefixForNamespace(namespace: String): Option[String] = {
    if (namespace == Scope.XmlNamespace) {
      Some("xml")
    } else {
      scope.prefixesForNamespace(namespace).lastOption
    }
  }

  /**
   * Returns the equivalent of `findPrefixForNamespace(namespace).get`.
   */
  def getPrefixForNamespace(namespace: String): String = {
    findPrefixForNamespace(namespace).getOrElse(sys.error(s"No prefix found for namespace '$namespace' using prefixed scope $this"))
  }

  /**
   * Calls `otherPrefixedScope.append(this)`.
   */
  def prepend(otherPrefixedScope: PrefixedScope): PrefixedScope = otherPrefixedScope.append(this)

  /**
   * Calls `PrefixedScope.from(this.scope.append(otherPrefixedScope.scope))`.
   *
   * Clearly, if a prefix occurs both in this PrefixedScope and in the parameter PrefixedScope, the prefix-namespace mapping in the parameter
   * PrefixedScope wins for that prefix.
   *
   * The resulting PrefixedScope is always a super-scope of the parameter PrefixedScope.
   *
   * Note that namespaces may get lost due to changed namespace bindings for some prefix, but prefixes never get
   * lost in the result. That's a desirable property when using type PrefixedScope in an element creation DSL, to
   * prevent the occurrence of prefixed namespace undeclarations, which are not allowed in XML 1.0.
   *
   * Also note that if method getPrefixForNamespace called on the parameter PrefixedScope for some namespace returns a certain prefix,
   * the same prefix for that namespace will be returned by a call of method getPrefixForNamespace on the result PrefixedScope.
   */
  def append(otherPrefixedScope: PrefixedScope): PrefixedScope = {
    PrefixedScope
      .from(this.scope.append(otherPrefixedScope.scope))
      .ensuring(_.superScopeOf(otherPrefixedScope))
  }

  /**
   * Returns `!conflictsWith(otherPrefixedScope)`.
   */
  def doesNotConflictWith(otherPrefixedScope: PrefixedScope): Boolean = scope.doesNotConflictWith(otherPrefixedScope.scope)

  /**
   * Returns true if there is at least one prefix for which this PrefixedScope and the parameter PrefixedScope disagree
   * on the namespace.
   */
  def conflictsWith(otherPrefixedScope: PrefixedScope): Boolean = scope.conflictsWith(otherPrefixedScope.scope)
}

object PrefixedScope {

  def from(scope: Scope): PrefixedScope = {
    require(scope.defaultNamespaceOption.isEmpty, s"Not a scope without default namespace: $scope")

    PrefixedScope(scope)
  }

  def optionallyFrom(scope: Scope): Option[PrefixedScope] = {
    if (scope.defaultNamespaceOption.isEmpty) Some(PrefixedScope.from(scope)) else None
  }

  def ignoringDefaultNamespace(scope: Scope): PrefixedScope = {
    from(scope.withoutDefaultNamespace)
  }

  /** The "empty" `PrefixedScope` */
  val empty: PrefixedScope = PrefixedScope(Scope.Empty)

  /**
   * Same as `PrefixedScope.from(Scope.from(m))`.
   */
  def from(m: ListMap[String, String]): PrefixedScope = {
    PrefixedScope.from(Scope.from(m))
  }

  /** Returns `from(Map[String, String](m: _*))` */
  def from(m: (String, String)*): PrefixedScope = from(ListMap[String, String](m: _*))
}
