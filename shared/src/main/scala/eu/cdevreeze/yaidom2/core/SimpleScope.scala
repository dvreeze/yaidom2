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
 * @author Chris de Vreeze
 */
final case class SimpleScope private (scope: Scope) {
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
   * Appends the parameter simple scope to this one, but throws an exception if they are conflicting in certain prefixes.
   */
  def append(otherSimpleScope: SimpleScope): SimpleScope = {
    val commonPrefixes = this.keySet.intersect(otherSimpleScope.keySet)
    require(
      this.filterKeys(commonPrefixes) == otherSimpleScope.filterKeys(commonPrefixes),
      s"Simple scope $otherSimpleScope cannot be appended to simple scope $this")

    SimpleScope.from(scope.append(otherSimpleScope.scope))
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
}