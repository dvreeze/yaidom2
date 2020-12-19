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
import scala.util.chaining._

/**
 * A holder of a Scope that, when ignoring the optional default namespace, is invertible. At least as importantly, stable scopes
 * are defined in terms of the operations allowed on them. The idea is to use stable scopes in element creation APIs, and
 * having the stable scopes grow in a "compatible" way.
 *
 * Stable scopes are compatible if they are non-conflicting (that is, common prefixes map to the same namespaces), while
 * agreeing on the presence or absence of the default namespace. In element creation APIs stable scopes "grow" in a compatible
 * way, by appending non-conflicting stable scopes returning compatible super-scopes (while not introducing a default namespace),
 * in particular by appending compatible stable scopes.
 *
 * When using stable scopes, start with deciding whether a default namespace is needed. If so, when appending stable
 * scopes this default namespace always remains there. If not, when appending stable scopes the default namespace cannot be added
 * anywhere in a compatible way. So typically this is a decision to take per document to create using an element creation API.
 *
 * If compatible stable scopes are used throughout an element tree, then all namespace declarations can be pushed up
 * to the root element of the tree.
 *
 * @author Chris de Vreeze
 */
final case class StableScope private (scope: Scope) {
  assert(scope.withoutDefaultNamespace.isInvertible)

  override def toString: String = scope.toString

  def defaultNamespaceOption: Option[String] = scope.defaultNamespaceOption

  /**
   * Returns true if this is a compatible sub-scope of the given parameter `StableScope`. A `StableScope` is a compatible sub-scope of itself.
   */
  def isCompatibleSubScopeOf(otherStableScope: StableScope): Boolean = {
    subScopeOf(otherStableScope) && this.defaultNamespaceOption == otherStableScope.defaultNamespaceOption
  }

  /**
   * Returns true if this is a compatible super-scope of the given parameter `StableScope`. A `StableScope` is a compatible super-scope of itself.
   */
  def isCompatibleSuperScopeOf(otherStableScope: StableScope): Boolean = otherStableScope.isCompatibleSubScopeOf(this)

  /**
   * Returns `StableScope.from(scope.filter(p).append(scope.retainingDefaultNamespace))`, so the result is a compatible sub-scope.
   */
  def filterCompatibly(p: ((String, String)) => Boolean): StableScope = {
    StableScope
      .from(scope.filter(p).append(scope.retainingDefaultNamespace))
      .ensuring(_.isCompatibleSubScopeOf(this))
  }

  /**
   * Returns `StableScope.from(scope.filterKeys(p).append(scope.retainingDefaultNamespace))`, so the result is a compatible sub-scope.
   */
  def filterKeysCompatibly(p: String => Boolean): StableScope = {
    StableScope
      .from(scope.filterKeys(p).append(scope.retainingDefaultNamespace))
      .ensuring(_.isCompatibleSubScopeOf(this))
  }

  /** Returns `scope.keySet`. */
  def keySet: Set[String] = scope.keySet

  /** Returns `scope.namespaces`. Hence, the "XML namespace" is not returned. */
  def namespaces: Set[String] = scope.namespaces

  /**
   * Returns `StableScope.from(scope.filterNamespaces(p).append(scope.retainingDefaultNamespace))`, so the result is a compatible sub-scope.
   */
  def filterNamespacesCompatibly(p: String => Boolean): StableScope = {
    StableScope
      .from(scope.filterNamespaces(p).append(scope.retainingDefaultNamespace))
      .ensuring(_.isCompatibleSubScopeOf(this))
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
   * Returns true if the parameter stable scope does not conflict with this stable scope, and both stable scopes either have
   * a default namespace (the same one, necessarily) or do not have any default namespace.
   */
  def isCompatibleWith(otherStableScope: StableScope): Boolean = {
    this.defaultNamespaceOption == otherStableScope.defaultNamespaceOption && this.doesNotConflictWith(otherStableScope)
  }

  /**
   * Returns true if this stable scope does not conflict with the parameter stable scopes. This is required for compatibility
   * of the stable scopes, but is a weaker property than compatibility.
   */
  def doesNotConflictWith(otherStableScope: StableScope): Boolean = {
    this.scope.doesNotConflictWith(otherStableScope.scope)
  }

  /**
   * Alias for `isCompatibleWith`, expressing in the method name that the `appendCompatibleScope` operation will be successful.
   */
  def canAppendCompatibleScope(otherStableScope: StableScope): Boolean = {
    isCompatibleWith(otherStableScope)
  }

  /**
   * Appends the parameter stable scope to this stable scope if they are compatible, returning the optional result.
   *
   * The resulting StableScope, if any, is always a compatible super-scope of the parameter StableScope and of this StableScope.
   * In practice the parameter stable scope is quite often a compatible sub-scope of this stable scope.
   *
   * There is no prependCompatibleScopeOption function, because appending a compatible scope is a commutative operation.
   */
  def appendCompatibleScopeOption(otherStableScope: StableScope): Option[StableScope] = {
    if (canAppendCompatibleScope(otherStableScope)) {
      val resultScope: StableScope =
        if (otherStableScope.isCompatibleSubScopeOf(this)) {
          this
        } else {
          StableScope.from(this.scope.append(otherStableScope.scope))
        }

      resultScope
        .ensuring(_.isCompatibleSuperScopeOf(this))
        .ensuring(_.isCompatibleSuperScopeOf(otherStableScope))
        .pipe(Some(_))
    } else {
      None
    }
  }

  /**
   * Calls the equivalent of  `appendCompatibleScopeOption(otherStableScope).get`.
   *
   * The resulting StableScope is always a compatible super-scope of the parameter StableScope and of this StableScope.
   */
  def appendCompatibleScope(otherStableScope: StableScope): StableScope = {
    appendCompatibleScopeOption(otherStableScope)
      .getOrElse(sys.error(s"Could not append stable scope $otherStableScope compatibly to stable scope $this"))
  }

  /**
   * Returns true if the parameter stable scope does not conflict with this stable scope, and the scope to be appended
   * either has no default namespace or it has the same default namespace as this stable scope.
   */
  def canAppendNonConflictingScope(otherStableScope: StableScope): Boolean = {
    val enhancedOtherStableScope: StableScope =
      if (otherStableScope.defaultNamespaceOption.isEmpty && this.defaultNamespaceOption.nonEmpty)
        StableScope.from(otherStableScope.scope.append(this.scope.retainingDefaultNamespace))
      else otherStableScope

    canAppendCompatibleScope(enhancedOtherStableScope)
  }

  /**
   * Appends the parameter stable scope to this stable scope if method `canAppendNonConflictingScope` returns true, returning the optional result.
   *
   * The resulting StableScope, if any, is always a super-scope of the parameter StableScope and a compatible super-scope of this StableScope.
   * In practice the parameter stable scope is quite often a sub-scope of this stable scope.
   */
  def appendNonConflictingScopeOption(otherStableScope: StableScope): Option[StableScope] = {
    if (canAppendNonConflictingScope(otherStableScope)) {
      val resultScope: StableScope =
        if (otherStableScope.subScopeOf(this)) {
          this
        } else {
          StableScope.from(this.scope.append(otherStableScope.scope))
        }

      resultScope
        .ensuring(_.isCompatibleSuperScopeOf(this))
        .ensuring(_.superScopeOf(otherStableScope))
        .pipe(Some(_))
    } else {
      None
    }
  }

  /**
   * Calls the equivalent of  `appendNonConflictingScopeOption(otherStableScope).get`.
   *
   * The resulting StableScope, is always a super-scope of the parameter StableScope and a compatible super-scope of this StableScope.
   */
  def appendNonConflictingScope(otherStableScope: StableScope): StableScope = {
    appendNonConflictingScopeOption(otherStableScope)
      .getOrElse(sys.error(s"Could not append non-conflicting stable scope $otherStableScope to stable scope $this"))
  }

  /** Returns true if this is a sub-scope of the given parameter `StableScope`. A `StableScope` is considered sub-scope of itself. */
  private def subScopeOf(otherStableScope: StableScope): Boolean = {
    scope.subScopeOf(otherStableScope.scope)
  }

  /** Returns true if this is a super-scope of the given parameter `StableScope`. A `StableScope` is considered super-scope of itself. */
  private def superScopeOf(otherStableScope: StableScope): Boolean = {
    scope.superScopeOf(otherStableScope.scope)
  }
}

object StableScope {

  def from(scope: Scope): StableScope = {
    require(scope.withoutDefaultNamespace.isInvertible, s"Not a scope that is invertible when ignoring the default namespace: $scope")

    StableScope(scope)
  }

  def optionallyFrom(scope: Scope): Option[StableScope] = {
    if (scope.withoutDefaultNamespace.isInvertible) Some(StableScope.from(scope)) else None
  }

  /** The "empty" `StableScope`. It contains no default namespace, and no default namespace can be added compatibly. */
  val empty: StableScope = StableScope(Scope.Empty)

  /** Creates a StableScope containing only a default namespace. This default namespace remains when appending stable scopes. */
  def fromDefaultNamespace(ns: String): StableScope = StableScope(Scope.from("" -> ns))

  /**
   * Same as `StableScope.from(Scope.from(m))`.
   */
  def from(m: ListMap[String, String]): StableScope = {
    StableScope.from(Scope.from(m))
  }

  /** Returns `from(Map[String, String](m: _*))` */
  def from(m: (String, String)*): StableScope = from(ListMap[String, String](m: _*))
}
