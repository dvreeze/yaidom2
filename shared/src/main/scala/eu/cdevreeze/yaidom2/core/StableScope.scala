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
 * A holder of a Scope that, when ignoring the optional default namespace, is invertible. A stable scopes can only be
 * appended to another one if both scopes agree on the namespace of each prefix (including the empty one) that they share,
 * and if both scopes either have a default namespace (the same one, necessarily) or don't have a default namespace.
 * In practice this means that the default namespace must be chosen beforehand, before starting with a stable scope and
 * appending other ones to it.
 *
 * Hence, the notion of a stable scope is defined partly by restrictions on the underlying scope, and partly by the operations
 * that are allowed on stable scopes (returning other stable scopes). This makes stable scopes predictable in practice.
 * In particular, a StableScope is useful for element creation DSLs. Note that despite the restrictions on stable scopes,
 * these restrictions are quite reasonable in practice for element creation APIs.
 *
 * If "compatible" stable scopes are used throughout an element tree, then all namespace declarations can be pushed up
 * to the root element of the tree. "Compatible" in this context means: there is one "start" stable scope and the other
 * stable scopes are the result of (directly or indirectly) appending other stable scopes to that start stable scope.
 *
 * @author Chris de Vreeze
 */
final case class StableScope private (scope: Scope) {
  assert(scope.withoutDefaultNamespace.isInvertible)

  override def toString: String = scope.toString

  def defaultNamespaceOption: Option[String] = scope.defaultNamespaceOption

  def retainingDefaultNamespace: StableScope = filterKeys(Set(Scope.DefaultNsPrefix))

  /** Returns true if this is a subscope of the given parameter `StableScope`. A `StableScope` is considered subscope of itself. */
  def subScopeOf(otherStableScope: StableScope): Boolean = {
    scope.subScopeOf(otherStableScope.scope)
  }

  /** Returns true if this is a superscope of the given parameter `StableScope`. A `StableScope` is considered superscope of itself. */
  def superScopeOf(otherStableScope: StableScope): Boolean = otherStableScope.subScopeOf(this)

  /** Returns true if this is a compatible subscope of the given parameter `StableScope`. A `StableScope` is a compatible subscope of itself. */
  def isCompatibleSubScopeOf(otherStableScope: StableScope): Boolean = {
    subScopeOf(otherStableScope) && this.defaultNamespaceOption == otherStableScope.defaultNamespaceOption
  }

  /** Returns true if this is a compatible superscope of the given parameter `StableScope`. A `StableScope` is a compatible superscope of itself. */
  def isCompatibleSuperScopeOf(otherStableScope: StableScope): Boolean = otherStableScope.isCompatibleSubScopeOf(this)

  /** Returns `StableScope.from(scope.filter(p))`. */
  def filter(p: ((String, String)) => Boolean): StableScope = StableScope.from(scope.filter(p))

  /** Returns `StableScope.from(scope.filterKeys(p))`. */
  def filterKeys(p: String => Boolean): StableScope = {
    StableScope.from(scope.filterKeys(p))
  }

  /** Returns `scope.keySet`. */
  def keySet: Set[String] = scope.keySet

  /** Returns `scope.namespaces`. Hence, the "XML namespace" is not returned. */
  def namespaces: Set[String] = scope.namespaces

  /** Returns `StableScope.from(scope.filterNamespaces(p))`. */
  def filterNamespaces(p: String => Boolean): StableScope = {
    StableScope.from(scope.filterNamespaces(p))
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
   * a default namespace (the same one, necessarily) or do not have a default namespace.
   */
  def isCompatibleWith(otherStableScope: StableScope): Boolean = {
    this.defaultNamespaceOption == otherStableScope.defaultNamespaceOption && this.scope.doesNotConflictWith(otherStableScope.scope)
  }

  /**
   * Alias for `isCompatibleWith`, expressing in the method name that the `append` operation will be successful.
   */
  def canAppend(otherStableScope: StableScope): Boolean = {
    isCompatibleWith(otherStableScope)
  }

  /**
   * Appends the parameter stable scope to this stable scope if it can be appended, returning the optional result.
   *
   * The resulting StableScope, if any, is always a super-scope of the parameter StableScope and of this StableScope.
   *
   * There is no prependOption function, because appending is a commutative operation.
   */
  def appendOption(otherStableScope: StableScope): Option[StableScope] = {
    if (canAppend(otherStableScope)) {
      val resultScope: StableScope =
        if (this == otherStableScope) {
          this
        } else {
          StableScope.from(this.scope.append(otherStableScope.scope))
        }

      resultScope
        .ensuring(_.superScopeOf(this))
        .ensuring(_.superScopeOf(otherStableScope))
        .ensuring(_.defaultNamespaceOption == this.defaultNamespaceOption)
        .ensuring(_.defaultNamespaceOption == otherStableScope.defaultNamespaceOption)
        .pipe(Some(_))
    } else {
      None
    }
  }

  /**
   * Calls the equivalent of  `appendOption(otherStableScope).get`.
   *
   * The resulting StableScope is always a super-scope of the parameter StableScope and of this StableScope.
   */
  def append(otherStableScope: StableScope): StableScope = {
    appendOption(otherStableScope)
      .getOrElse(sys.error(s"Could not append stable scope $otherStableScope to stable scope $this"))
  }

  /**
   * Returns true if the parameter stable scope does not conflict with this stable scope, and the scope to be appended
   * either has no default namespace or it has the same default namespace as this stable scope.
   *
   * The word "unsafe" means that both scopes may not agree on the existence of a default namespace, which can be a problem
   * when creating element trees using appended scopes, where potentially only some elements in the tree may have a default namespace.
   */
  def canAppendUnsafely(otherStableScope: StableScope): Boolean = {
    val enhancedOtherStableScope: StableScope =
      if (otherStableScope.defaultNamespaceOption.isEmpty && this.defaultNamespaceOption.nonEmpty)
        StableScope.from(otherStableScope.scope.append(this.scope.retainingDefaultNamespace))
      else otherStableScope

    canAppend(enhancedOtherStableScope)
  }

  /**
   * Appends the parameter stable scope to this stable scope if it can be appended unsafely, returning the optional result.
   *
   * The resulting StableScope, if any, is always a super-scope of the parameter StableScope and of this StableScope.
   *
   * The word "unsafe" means that both scopes may not agree on the existence of a default namespace, which can be a problem
   * when creating element trees using appended scopes, where potentially only some elements in the tree may have a default namespace.
   */
  def appendUnsafelyOption(otherStableScope: StableScope): Option[StableScope] = {
    if (canAppendUnsafely(otherStableScope)) {
      val resultScope: StableScope =
        if (this == otherStableScope) {
          this
        } else {
          StableScope.from(this.scope.append(otherStableScope.scope))
        }

      resultScope
        .ensuring(_.superScopeOf(this))
        .ensuring(_.superScopeOf(otherStableScope))
        .ensuring(_.defaultNamespaceOption == this.defaultNamespaceOption)
        .pipe(Some(_))
    } else {
      None
    }
  }

  /**
   * Calls the equivalent of  `appendUnsafelyOption(otherStableScope).get`.
   *
   * The resulting StableScope is always a super-scope of the parameter StableScope and of this StableScope.
   *
   * The word "unsafe" means that both scopes may not agree on the existence of a default namespace, which can be a problem
   * when creating element trees using appended scopes, where potentially only some elements in the tree may have a default namespace.
   */
  def appendUnsafely(otherStableScope: StableScope): StableScope = {
    appendUnsafelyOption(otherStableScope)
      .getOrElse(sys.error(s"Could not append stable scope $otherStableScope unsafely to stable scope $this"))
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

  /** The "empty" `StableScope` */
  val empty: StableScope = StableScope(Scope.Empty)

  /**
   * Same as `StableScope.from(Scope.from(m))`.
   */
  def from(m: ListMap[String, String]): StableScope = {
    StableScope.from(Scope.from(m))
  }

  /** Returns `from(Map[String, String](m: _*))` */
  def from(m: (String, String)*): StableScope = from(ListMap[String, String](m: _*))
}
