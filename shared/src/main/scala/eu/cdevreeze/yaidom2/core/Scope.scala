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
 * Scope mapping prefixes to namespace URIs, as well as holding an optional default namespace. In other words, <em>in-scope
 * namespaces</em>.
 *
 * The purpose of a [[eu.cdevreeze.yaidom2.core.Scope]] is to resolve [[eu.cdevreeze.yaidom2.core.QName]]s as [[eu.cdevreeze.yaidom2.core.EName]]s.
 *
 * For example, consider the following XML:
 * {{{
 * <book:Bookstore xmlns:book="http://bookstore/book">
 *   <book:Book ISBN="978-0321356680" Price="35" Edition="2">
 *     <book:Title>Effective Java (2nd Edition)</book:Title>
 *     <book:Authors>
 *       <auth:Author xmlns:auth="http://bookstore/author">
 *         <auth:First_Name>Joshua</auth:First_Name>
 *         <auth:Last_Name>Bloch</auth:Last_Name>
 *       </auth:Author>
 *     </book:Authors>
 *   </book:Book>
 * </book:Bookstore>
 * }}}
 * Then the (only) author element has the following scope:
 * {{{
 * Scope.from("book" -> "http://bookstore/book", "auth" -> "http://bookstore/author")
 * }}}
 *
 * After all, the root element has the following scope:
 * {{{
 * Scope.Empty.resolve(Declarations.from("book" -> "http://bookstore/book"))
 * }}}
 * which is the same as:
 * {{{
 * Scope.from("book" -> "http://bookstore/book")
 * }}}
 *
 * The (only) book element has no namespace declarations, so it has the same scope. That is also true for the authors element
 * inside the book element. The (only) author element introduces a new namespace, and its scope is as follows:
 * {{{
 * Scope.from("book" -> "http://bookstore/book").resolve(Declarations.from("auth" -> "http://bookstore/author"))
 * }}}
 * which is indeed:
 * {{{
 * Scope.from("book" -> "http://bookstore/book", "auth" -> "http://bookstore/author")
 * }}}
 *
 * The author element `QName("auth:Author")` has (optional) resolved name:
 * {{{
 * Scope.from("book" -> "http://bookstore/book", "auth" -> "http://bookstore/author").resolveQNameOption(QName("auth:Author"))
 * }}}
 * which is:
 * {{{
 * Some(EName("{http://bookstore/author}Author"))
 * }}}
 *
 * A `Scope` must not contain prefix "xmlns" and must not contain namespace URI "http://www.w3.org/2000/xmlns/".
 * Moreover, a `Scope` must not contain the XML namespace (prefix "xml", namespace URI "http://www.w3.org/XML/1998/namespace").
 *
 * The Scope is backed by a SeqMap from prefixes (or the empty string for the default namespace) to (non-empty) namespace URIs.
 *
 * This class depends on Declarations, but not the other way around.
 *
 * Implementation note: this class used a ListMap instead of VectorMap (via the SeqMap API), due to Scala issue
 * https://github.com/scala/scala/pull/8854.
 *
 * @author Chris de Vreeze
 */
final case class Scope(prefixNamespaceMap: ListMap[String, String]) {
  require(!prefixNamespaceMap.contains("xml"), s"Prefix 'xml' is built-in and must not occur explicitly in the scope")

  import Scope._

  /** Returns true if this Scope is empty. Faster than comparing this Scope against the empty Scope. */
  def isEmpty: Boolean = prefixNamespaceMap.isEmpty

  /** Returns true if this Scope is not empty. */
  def nonEmpty: Boolean = !isEmpty

  /** Returns the default namespace, if any, wrapped in an Option */
  def defaultNamespaceOption: Option[String] = prefixNamespaceMap.get(DefaultNsPrefix)

  /** Returns an adapted copy of this Scope, but retaining only the default namespace, if any */
  def retainingDefaultNamespace: Scope = {
    val m = prefixNamespaceMap.filter { case (pref, _) => pref == DefaultNsPrefix }
    if (m.isEmpty) Scope.Empty else Scope(m)
  }

  /** Returns an adapted copy of this Scope, but without the default namespace, if any */
  def withoutDefaultNamespace: Scope = if (defaultNamespaceOption.isEmpty) this else Scope(prefixNamespaceMap - DefaultNsPrefix)

  /**
   * Returns true if the inverse exists, that is, each namespace URI has a unique prefix
   * (including the empty prefix for the default namespace, if applicable).
   *
   * In other words, returns true if the inverse of `toMap` is also a mathematical function, mapping namespace URIs to unique prefixes.
   *
   * Invertible scopes offer a one-to-one correspondence between QNames and ENames. This is needed, for example, for `Path`s.
   * Only if there is such a one-to-one correspondence, the indexes in `Path`s and `PathBuilder`s are stable, when converting
   * between the two.
   */
  def isInvertible: Boolean = prefixNamespaceMap.keySet.size == namespaces.size

  /** Returns true if this is a subscope of the given parameter `Scope`. A `Scope` is considered subscope of itself. */
  def subScopeOf(scope: Scope): Boolean = {
    val thisMap = prefixNamespaceMap
    val otherMap = scope.prefixNamespaceMap

    thisMap.keySet.subsetOf(otherMap.keySet) && {
      thisMap.keySet.forall { pref =>
        thisMap(pref) == otherMap(pref)
      }
    }
  }

  /** Returns true if this is a superscope of the given parameter `Scope`. A `Scope` is considered superscope of itself. */
  def superScopeOf(scope: Scope): Boolean = scope.subScopeOf(this)

  /** Returns `Scope.from(this.prefixNamespaceMap.filter(p))`. */
  def filter(p: ((String, String)) => Boolean): Scope = Scope.from(this.prefixNamespaceMap.filter(p))

  /** Returns `Scope.from(this.prefixNamespaceMap.filterKeys(p))`. */
  def filterKeys(p: String => Boolean): Scope = {
    // Method filterKeys deprecated since Scala 2.13.0.
    Scope.from(this.prefixNamespaceMap.filter { case (pref, _) => p(pref) }.to(ListMap))
  }

  /** Returns `this.prefixNamespaceMap.keySet`. */
  def keySet: Set[String] = this.prefixNamespaceMap.keySet

  /** Returns `this.prefixNamespaceMap.values.toSet`. Hence, the "XML namespace" is not returned. */
  def namespaces: Set[String] = this.prefixNamespaceMap.values.toSet

  /** Returns `filter(kv => p(kv._2))`. */
  def filterNamespaces(p: String => Boolean): Scope = {
    filter(kv => p(kv._2))
  }

  /**
   * Returns `resolveQNameOption(qname).get`.
   *
   * TODO Create type ENameProvider, and use implicit ENameProvider.
   */
  def resolveQName(qname: QName): EName = {
    resolveQNameOption(qname)
      .getOrElse(sys.error(s"Could not resolve QName '$qname' in scope '${Scope.this}'"))
  }

  /**
   * Tries to resolve the given `QName` against this `Scope`, returning `None` for prefixed names whose prefixes are unknown
   * to this `Scope`.
   *
   * Note that the `subScopeOf` relation keeps the `resolveQNameOption` result the same, provided there is no default namespace.
   * That is, if `scope1.withoutDefaultNamespace.subScopeOf(scope2.withoutDefaultNamespace)`, then for each QName `qname`
   * such that `scope1.withoutDefaultNamespace.resolveQNameOption(qname).isDefined`, we have:
   * {{{
   * scope1.withoutDefaultNamespace.resolveQNameOption(qname) == scope2.withoutDefaultNamespace.resolveQNameOption(qname)
   * }}}
   *
   * TODO Create type ENameProvider, and use implicit ENameProvider.
   */
  def resolveQNameOption(qname: QName): Option[EName] = {
    qname match {
      case unprefixedName: UnprefixedName if defaultNamespaceOption.isEmpty =>
        Some(EName(None, unprefixedName.localPart))
      case unprefixedName: UnprefixedName =>
        Some(EName(defaultNamespaceOption, unprefixedName.localPart))
      case PrefixedName(prefix, localPart) =>
        // Thanks to Johan Walters for pointing to a performance bottleneck in previous versions of this code (in yaidom)
        prefix match {
          case "xml" => Some(EName(Some(XmlNamespace), localPart))
          case ""    => None // Quirk to keep old behavior of "allowing" empty prefixes and ignoring them when resolving the name
          case _     => prefixNamespaceMap.get(prefix).map(nsUri => EName(Some(nsUri), localPart))
        }
    }
  }

  /**
   * Resolves the given declarations against this `Scope`, returning an "updated" `Scope`.
   *
   * Inspired by `java.net.URI`, which has a similar method for URIs.
   */
  def resolve(declarations: Declarations): Scope = {
    if (declarations.isEmpty) {
      this
    } else {
      val declared: Declarations = declarations.withoutUndeclarations
      val undeclarations: Declarations = declarations.retainingUndeclarations

      assert(declared.prefixNamespaceMap.keySet.intersect(undeclarations.prefixNamespaceMap.keySet).isEmpty)
      val m = (prefixNamespaceMap ++ declared.prefixNamespaceMap) -- undeclarations.prefixNamespaceMap.keySet
      Scope(m)
    }
  }

  /**
   * Relativizes the given `Scope` against this `Scope`, returning a `Declarations` object.
   *
   * Inspired by `java.net.URI`, which has a similar method for URIs.
   */
  def relativize(scope: Scope): Declarations = {
    if (Scope.this == scope) {
      Declarations.Empty
    } else {
      val newlyDeclared: ListMap[String, String] = scope.prefixNamespaceMap.filter {
        case (pref, ns) =>
          assert(ns.nonEmpty)
          Scope.this.prefixNamespaceMap.getOrElse(pref, "") != ns
      }

      val removed: Set[String] = Scope.this.prefixNamespaceMap.keySet.diff(scope.prefixNamespaceMap.keySet)
      val undeclarations: Map[String, String] = removed.map(pref => pref -> "").toMap

      assert(newlyDeclared.keySet.intersect(removed).isEmpty)
      val m: ListMap[String, String] = newlyDeclared ++ undeclarations

      Declarations(m)
    }
  }

  /**
   * Returns the smallest sub-declarations `decl` of `declarations` such that `this.resolve(decl) == this.resolve(declarations)`
   */
  def minimize(declarations: Declarations): Declarations = {
    val declared = declarations.withoutUndeclarations.prefixNamespaceMap.filter {
      case (pref, ns) => this.prefixNamespaceMap.getOrElse(pref, "") != ns
    }
    val undeclared = declarations.retainingUndeclarations.prefixNamespaceMap.keySet.intersect(this.prefixNamespaceMap.keySet)

    val result = Declarations(declared) ++ Declarations.undeclaring(undeclared)

    assert(this.resolve(declarations) == this.resolve(result))
    result
  }

  /**
   * Returns `Scope(this.prefixNamespaceMap ++ scope.prefixNamespaceMap)`. Note that namespaces may get lost as a result.
   *
   * Clearly, if a prefix occurs both in this Scope and in the parameter Scope, the prefix-namespace mapping in the parameter
   * Scope wins for that prefix.
   */
  def append(scope: Scope): Scope = Scope(this.prefixNamespaceMap ++ scope.prefixNamespaceMap)

  /** Returns `scope.append(this)` */
  def prepend(scope: Scope): Scope = scope.append(this)

  /** Returns `Scope(this.prefixNamespaceMap -- prefixes)` */
  def minus(prefixes: Set[String]): Scope = Scope(this.prefixNamespaceMap -- prefixes)

  /** Alias for `append` */
  // scalastyle:off method.name
  def ++(scope: Scope): Scope = append(scope)

  /** Alias for `minus` */
  // scalastyle:off method.name
  def --(prefixes: Set[String]): Scope = minus(prefixes)

  /** Alias for `append(Scope.from((prefix, namespace)))` */
  def append(prefix: String, namespace: String): Scope = append(Scope.from((prefix, namespace)))

  /** Creates a `String` representation of this `Scope`, as it is shown in XML */
  def toStringInXml: String = {
    val defaultNsString = if (defaultNamespaceOption.isEmpty) "" else """xmlns="%s"""".format(defaultNamespaceOption.get)
    val prefixScopeString =
      (prefixNamespaceMap - DefaultNsPrefix).map { case (pref, ns) => """xmlns:%s="%s"""".format(pref, ns) }.mkString(" ")
    List(defaultNsString, prefixScopeString).filterNot { _ == "" }.mkString(" ")
  }

  /**
   * Returns the inverse of this Scope, as Map from namespace URIs to collections of prefixes. These prefixes also include
   * the empty String if this Scope has a default namespace.
   */
  def inverse: Map[String, Set[String]] = {
    val nsPrefixPairs = this.prefixNamespaceMap.toSeq.map { case (prefix, ns)                               => (ns, prefix) }
    val nsPrefixPairsGroupedByNs: Map[String, Seq[(String, String)]] = nsPrefixPairs.groupBy { case (ns, _) => ns }

    // Method mapValues deprecated since Scala 2.13.0.
    val result = nsPrefixPairsGroupedByNs.toSeq.map {
      case (ns, xs) =>
        val result = xs.map { case (_, prefix) => prefix }
        ns -> result.toSet
    }.toMap

    assert(result.values.forall(_.nonEmpty))
    result
  }

  /**
   * Returns an invertible Scope having the same namespaces but only one prefix per namespace.
   * If this Scope has a default namespace, the returned Scope possibly has that default namespace
   * as well.
   */
  def makeInvertible: Scope = {
    // Method mapValues deprecated since Scala 2.13.0.
    val prefixNsMap = inverse.toSeq
      .map { case (ns, prefs) => ns -> prefs.head }
      .map(_.swap)
      .to(ListMap)

    Scope.from(prefixNsMap).ensuring(_.isInvertible)
  }

  /**
   * Returns the prefixes for the given namespace URI (in insertion order). The result includes the empty string for the default
   * namespace, if the default namespace is indeed equal to the passed namespace URI. The result does not include "xml" for the
   * implicit "xml" namespace (with namespace URI http://www.w3.org/XML/1998/namespace).
   *
   * The result, converted to a Set, is equivalent to:
   * {{{
   * this.inverse.getOrElse(namespaceUri, Set())
   * }}}
   *
   * This method can be handy when "inserting" an "element" into a parent tree, if one wants to reuse prefixes of the
   * parent tree.
   */
  def prefixesForNamespace(namespaceUri: String): Seq[String] = {
    require(namespaceUri.nonEmpty, s"Empty namespace URI not allowed")

    val prefixes: Seq[String] = this.prefixNamespaceMap.toSeq.collect { case (prefix, ns) if ns == namespaceUri => prefix }
    prefixes
  }

  /**
   * Returns one of the prefixes for the given namespace URI, if any, and otherwise falling back to a prefix (or exception)
   * computed by the 2nd parameter. The result may be the empty string for the default namespace, if
   * the default namespace is indeed equal to the passed namespace URI. The result is "xml" if the
   * namespace URI is "http://www.w3.org/XML/1998/namespace".
   *
   * If the given namespace is the default namespace, but if there is also a non-empty prefix for the namespace, that
   * non-empty prefix is returned. Otherwise, if the given namespace is the default namespace, the empty string is returned.
   * If there are multiple non-empty prefixes for the namespace, the last one (in insertion order) is returned.
   *
   * The prefix fallback is only used if `prefixesForNamespace(namespaceUri).isEmpty`. If the fallback prefix conflicts
   * with an already used prefix (including ""), an exception is thrown.
   *
   * This method can be handy when "inserting" an "element" into a parent tree, if one wants to reuse prefixes of the
   * parent tree.
   */
  def prefixForNamespace(namespaceUri: String, getFallbackPrefix: () => String): String = {
    require(namespaceUri.nonEmpty, s"Empty namespace URI not allowed")

    if (namespaceUri == XmlNamespace) {
      "xml"
    } else {
      val prefixes: Seq[String] = prefixesForNamespace(namespaceUri)

      if (prefixes.isEmpty) {
        val pref = getFallbackPrefix()
        require(pref != "xml", s"Fallback prefix $pref for namespace $namespaceUri not allowed")

        val foundNs = prefixNamespaceMap.getOrElse(pref, namespaceUri)
        if (foundNs != namespaceUri) {
          sys.error(s"Prefix $pref already bound to namespace $foundNs, so cannot be bound to $namespaceUri in the same scope")
        }

        pref
      } else {
        if (prefixes == Seq("")) "" else prefixes.filterNot(_ == "").last
      }
    }
  }

  /**
   * Convenience method, returning the equivalent of:
   * {{{
   * this.resolve(
   *   Declarations.from(prefixForNamespace(namespaceUri, getFallbackPrefix) -> namespaceUri))
   * }}}
   *
   * If the namespace is "http://www.w3.org/XML/1998/namespace", this Scope is returned.
   *
   * If the fallback prefix is used and conflicts with an already used prefix (including ""), an exception is thrown,
   * as documented for method `prefixForNamespace`.
   *
   * The following property holds:
   * {{{
   * this.subScopeOf(this.includingNamespace(namespaceUri, getFallbackPrefix))
   * }}}
   */
  def includingNamespace(namespaceUri: String, getFallbackPrefix: () => String): Scope = {
    require(namespaceUri.nonEmpty, s"Empty namespace URI not allowed")

    if (namespaceUri == XmlNamespace || prefixesForNamespace(namespaceUri).nonEmpty) {
      this
    } else {
      assert(namespaceUri != XmlNamespace)

      // Throws an exception if the prefix has already been bound to another namespace
      val prefix = prefixForNamespace(namespaceUri, getFallbackPrefix)
      this.resolve(Declarations.from(prefix -> namespaceUri))
    }
  }

  /**
   * Returns `!conflictsWith(otherScope)`.
   */
  def doesNotConflictWith(otherScope: Scope): Boolean = {
    !conflictsWith(otherScope)
  }

  /**
   * Returns true if there is at least one (possibly empty) prefix for which this Scope and the parameter Scope disagree
   * on the namespace.
   */
  def conflictsWith(otherScope: Scope): Boolean = {
    val commonPrefixes: Set[String] = this.keySet.intersect(otherScope.keySet)
    this.prefixNamespaceMap.view.filterKeys(commonPrefixes).toMap != otherScope.prefixNamespaceMap.view.filterKeys(commonPrefixes).toMap
  }
}

object Scope {

  /** The "empty" `Scope` */
  val Empty: Scope = Scope(ListMap())

  /**
   * Same as the constructor, but removing the 'xml' prefix, if any.
   * Therefore this call is easier to use than the constructor or default `apply` method.
   */
  def from(m: ListMap[String, String]): Scope = {
    if (m.contains("xml")) {
      require(m("xml") == XmlNamespace, "The 'xml' prefix must map to 'http://www.w3.org/XML/1998/namespace'")
    }
    Scope(m - "xml")
  }

  /** Returns `from(Map[String, String](m: _*))` */
  def from(m: (String, String)*): Scope = from(ListMap[String, String](m: _*))

  val DefaultNsPrefix = ""

  val XmlNamespace: String = Declarations.XmlNamespace
}
