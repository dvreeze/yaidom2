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

/**
 * A holder of a Scope that is invertible and has no default namespace. It is useful for element creation DSLs, for example.
 *
 * Simple scopes are far more predictable than scopes in general. For example, by being invertible they support a one-to-one
 * correspondence between ENames and QNames, which is an attractive property when building element trees without worrying
 * too much about QNames (while those QNames must still be generated at some point). By not containing any default namespace
 * unprefixed QNames always have no namespace, and cannot suddenly and silently get a namespace by the appearance of a
 * default namespace anywhere in the element ancestry. Hence, simple scopes are preferable to scopes in general when used
 * in element creation DSLs.
 *
 * @author Chris de Vreeze
 */
final case class SimpleScope private(scope: Scope) {
  assert(scope.isInvertible)
  assert(scope.defaultNamespaceOption.isEmpty)

  override def toString: String = scope.toString

  /** Returns true if this is a subscope of the given parameter `SimpleScope`. A `SimpleScope` is considered subscope of itself. */
  def subScopeOf(otherSimpleScope: SimpleScope): Boolean = {
    scope.subScopeOf(otherSimpleScope.scope)
  }

  /** Returns true if this is a superscope of the given parameter `SimpleScope`. A `SimpleScope` is considered superscope of itself. */
  def superScopeOf(otherSimpleScope: SimpleScope): Boolean = otherSimpleScope.subScopeOf(this)

  /** Returns `SimpleScope.from(scope.filter(p))`. */
  def filter(p: ((String, String)) => Boolean): SimpleScope = SimpleScope.from(scope.filter(p))

  /** Returns `SimpleScope.from(scope.filterKeys(p))`. */
  def filterKeys(p: String => Boolean): SimpleScope = {
    SimpleScope.from(scope.filterKeys(p))
  }

  /** Returns `scope.keySet`. */
  def keySet: Set[String] = scope.keySet

  /** Returns `scope.namespaces`. Hence, the "XML namespace" is not returned. */
  def namespaces: Set[String] = scope.namespaces

  /** Returns `SimpleScope.from(scope.filterNamespaces(p))`. */
  def filterNamespaces(p: String => Boolean): SimpleScope = {
    SimpleScope.from(scope.filterNamespaces(p))
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
   * Finds the optional QName for the given EName.
   */
  def findQName(ename: EName): Option[QName] = {
    if (ename.namespaceUriOption.isEmpty) {
      Some(QName.fromLocalName(ename.localPart))
    } else {
      if (ename.namespaceUriOption.contains(Scope.XmlNamespace)) {
        Some(QName("xml", ename.localPart))
      } else {
        val prefixes: Set[String] = scope.prefixesForNamespace(ename.namespaceUriOption.get).ensuring(_.forall(_.nonEmpty))

        val prefixOption: Option[String] = prefixes.headOption

        prefixOption.map { prefix =>
          QName(prefix, ename.localPart)
        }
      }
    }.ensuring(_.forall(qn => scope.resolveQNameOption(qn).contains(ename)))
  }

  def findPrefixForNamespace(namespace: String): Option[String] = {
    if (namespace == Scope.XmlNamespace) {
      Some("xml")
    } else {
      scope.prefixesForNamespace(namespace).headOption
    }
  }

  /**
   * Appends the parameter simple scope to this one, but leaves out the prefixes and namespaces in the parameter simple scope
   * that occur in this simple scope. In other words, this simple scope takes precedence over the parameter simple scope.
   * As a consequence, it is possible that not all namespaces in the parameter simple scope end up in the result.
   * The result is always a super-scope of this simple scope.
   */
  def appendDefensively(otherSimpleScope: SimpleScope): SimpleScope = {
    val prefixesToKeep: Set[String] = this.keySet
    val namespacesToKeep: Set[String] = this.namespaces

    val scopeToAdd: Scope = otherSimpleScope.scope
      .filterKeys(pref => !prefixesToKeep.contains(pref))
      .filterNamespaces(ns => !namespacesToKeep.contains(ns))

    SimpleScope.from(this.scope.append(scopeToAdd))
      .ensuring(sc => this.subScopeOf(sc))
  }

  /**
   * Appends the parameter simple scope to this one, but leaves out the prefixes and namespaces in this (!) simple scope
   * that occur in the parameter simple scope. In other words, the parameter simple scope takes precedence over this one.
   * As a consequence, it is possible that not all namespaces in this simple scope end up in the result.
   * The result is always a super-scope of the parameter simple scope.
   */
  def appendAggressively(otherSimpleScope: SimpleScope): SimpleScope = {
    val prefixesToKeep: Set[String] = otherSimpleScope.keySet
    val namespacesToKeep: Set[String] = otherSimpleScope.namespaces

    val scopeToRetain: Scope = this.scope
      .filterKeys(pref => !prefixesToKeep.contains(pref))
      .filterNamespaces(ns => !namespacesToKeep.contains(ns))

    SimpleScope.from(scopeToRetain.append(otherSimpleScope.scope))
      .ensuring(sc => otherSimpleScope.subScopeOf(sc))
  }

  /**
   * Like `appendDefensively`, but throws an exception if namespaces get lost.
   */
  def appendDefensivelyOrThrow(otherSimpleScope: SimpleScope): SimpleScope = {
    val result = appendDefensively(otherSimpleScope)

    val lostNamespaces: Set[String] = (this.namespaces.union(otherSimpleScope.namespaces)).diff(result.namespaces)

    if (lostNamespaces.nonEmpty) {
      sys.error(s"Losing namespaces ${lostNamespaces.mkString(", ")} when adding $otherSimpleScope to $this (defensively)")
    }
    result
  }

  /**
   * Like `appendAggressively`, but throws an exception if namespaces get lost.
   */
  def appendAggressivelyOrThrow(otherSimpleScope: SimpleScope): SimpleScope = {
    val result = appendAggressively(otherSimpleScope)

    val lostNamespaces: Set[String] = (this.namespaces.union(otherSimpleScope.namespaces)).diff(result.namespaces)

    if (lostNamespaces.nonEmpty) {
      sys.error(s"Losing namespaces ${lostNamespaces.mkString(", ")} when adding $otherSimpleScope to $this (aggressively)")
    }
    result
  }
}

object SimpleScope {

  def from(scope: Scope): SimpleScope = {
    require(scope.isInvertible, s"Not an invertible scope: $scope")
    require(scope.defaultNamespaceOption.isEmpty, s"Not a scope without default namespace: $scope")

    SimpleScope(scope)
  }

  def optionallyFrom(scope: Scope): Option[SimpleScope] = {
    if (scope.isInvertible && scope.defaultNamespaceOption.isEmpty) Some(SimpleScope.from(scope)) else None
  }

  /** The "empty" `SimpleScope` */
  val Empty = SimpleScope(Scope.Empty)

  /**
   * Same as `SimpleScope.from(Scope.from(m))`.
   */
  def from(m: Map[String, String]): SimpleScope = {
    SimpleScope.from(Scope.from(m))
  }

  /** Returns `from(Map[String, String](m: _*))` */
  def from(m: (String, String)*): SimpleScope = from(Map[String, String](m: _*))
}
