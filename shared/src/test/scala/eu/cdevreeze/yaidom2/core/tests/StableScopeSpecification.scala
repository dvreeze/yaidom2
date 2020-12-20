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

package eu.cdevreeze.yaidom2.core.tests

import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.core.StableScope
import eu.cdevreeze.yaidom2.core.tests.StableScopeSpecification._
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Properties

import scala.collection.immutable.ListMap

/**
 * StableScope scalacheck specification.
 *
 * @author Chris de Vreeze
 */
class StableScopeSpecification extends Properties("StableScope") {

  // Generators

  private val genPrefix: Gen[String] = Gen.alphaLowerStr.flatMap(s => Gen.choose(0, 12).map(n => s.take(n)))

  private val genNamespace: Gen[String] = {
    val genProtocol: Gen[String] = Gen.oneOf("http", "https")
    val genDomain: Gen[String] = Gen.alphaLowerStr.suchThat(_.nonEmpty).map(s => s"www.$s.com")
    val genPath: Gen[String] = Gen.alphaLowerStr.suchThat(_.nonEmpty)

    for {
      protocol <- genProtocol
      domain <- genDomain
      path <- genPath
    } yield s"$protocol://$domain/$path/"
  }

  private val genPrefNsPair: Gen[(String, String)] = {
    for {
      prefix <- genPrefix
      namespace <- genNamespace
    } yield (prefix, namespace)
  }

  private val genScope: Gen[Scope] = {
    for {
      prefixNamespaceMap <- Gen.containerOf[List, (String, String)](genPrefNsPair)
    } yield Scope.from(prefixNamespaceMap.filterNot(_._1 == "xml").to(ListMap))
  }

  private val genStableScope: Gen[StableScope] = {
    for {
      sc <- genScope.suchThat(_.withoutDefaultNamespace.isInvertible)
    } yield StableScope.from(sc)
  }

  private def makeInvertibleIgnoringDefaultNamespace(sc: Scope): Scope = {
    sc.withoutDefaultNamespace.makeInvertible.append(sc.retainingDefaultNamespace)
  }

  private val genStableScopePair: Gen[(StableScope, StableScope)] = {
    for {
      startScope <- genScope.suchThat(_.keySet.sizeIs >= 5)
      prefixes1 <- Gen.someOf(startScope.keySet).map(_.toSet)
      prefixes2 <- Gen.someOf(startScope.keySet).map(_.toSet)
      extraScope <- Gen.oneOf(Scope.Empty, Scope.from("a" -> "http://www.test.com/a"), Scope.from("a" -> "http://www.test.com/b"))
    } yield {
      // It is not unlikely that scope1 and scope2 have some overlapping prefixes, and they may or may not conflict
      val scope1: Scope = makeInvertibleIgnoringDefaultNamespace(startScope.filterKeys(prefixes1).append(extraScope))
      val scope2: Scope = makeInvertibleIgnoringDefaultNamespace(startScope.filterKeys(prefixes2).append(extraScope))
      (StableScope.from(scope1), StableScope.from(scope2))
    }
  }

  private val genStableScopeAndTwoCompatibleSubScopes: Gen[StableScopeAndTwoCompatibleSubScopes] = {
    for {
      superScope <- genScope
        .suchThat(_.keySet.sizeIs >= 5)
        .map(makeInvertibleIgnoringDefaultNamespace)
        .map(StableScope.from)
      prefixes1 <- Gen.someOf(superScope.keySet).map(_.toSet)
      subScope1 <- Gen.oneOf(Seq(superScope.filterKeysCompatibly(prefixes1)))
      prefixes2 <- Gen.someOf(superScope.keySet).map(_.toSet)
      subScope2 <- Gen.oneOf(Seq(superScope.filterKeysCompatibly(prefixes2)))
    } yield {
      StableScopeAndTwoCompatibleSubScopes(superScope, subScope1, subScope2)
    }
  }

  private val genStableScopeAndCompatibleSuperScopeAndExtraScope: Gen[StableScopeAndCompatibleSuperScopeAndExtraScope] = {
    for {
      superScope <- genScope
        .suchThat(_.keySet.sizeIs >= 5)
        .map(makeInvertibleIgnoringDefaultNamespace)
        .map(StableScope.from)
      prefixesOfSubScope <- Gen.someOf(superScope.keySet).map(_.toSet)
      subScope <- Gen
        .oneOf(Seq(superScope.scope.filterKeys(prefixesOfSubScope + "")))
        .map(StableScope.from)
        .map(_.ensuring(_.isCompatibleSubScopeOf(superScope)))
      partialExtraScope <- Gen.oneOf(Scope.Empty, Scope.from("a" -> "http://www.test.com/a"), Scope.from("a" -> "http://www.test.com/b"))
      prefixesToAddToExtraScope <- Gen
        .someOf(superScope.keySet)
        .map(_.toSet)
        .flatMap(prefs => Gen.oneOf[Int](0, 1, 3).map(n => prefs.take(n)))
      extraScope <- Gen
        .oneOf(Seq(superScope.scope.filterKeys(prefixesToAddToExtraScope)))
        .map(_.append(partialExtraScope))
        .map(makeInvertibleIgnoringDefaultNamespace)
        .map(StableScope.from)
        .suchThat(sc => superScope.canAppendNonConflictingScope(sc))
    } yield {
      StableScopeAndCompatibleSuperScopeAndExtraScope(
        StableScopeAndCompatibleSuperScope(subScope, superScope),
        extraScope
      )
    }
  }

  implicit val arbitraryScope: Arbitrary[Scope] = {
    Arbitrary(genScope)
  }

  implicit val arbitraryStableScope: Arbitrary[StableScope] = {
    Arbitrary(genStableScope)
  }

  implicit val arbitraryStableScopePair: Arbitrary[(StableScope, StableScope)] = {
    Arbitrary(genStableScopePair)
  }

  implicit val arbitraryStableScopeAndTwoCompatibleSubScopes: Arbitrary[StableScopeAndTwoCompatibleSubScopes] = {
    Arbitrary(genStableScopeAndTwoCompatibleSubScopes)
  }

  implicit val arbitraryStableScopeAndCompatibleSuperScopeAndExtraScope: Arbitrary[StableScopeAndCompatibleSuperScopeAndExtraScope] = {
    Arbitrary(genStableScopeAndCompatibleSuperScopeAndExtraScope)
  }

  // Properties ("definitions", "theorems", etc.)

  // "Definitions"

  property("optionallyFrom") = forAll { scope: Scope =>
    StableScope.optionallyFrom(scope) == {
      if (scope.withoutDefaultNamespace.isInvertible) Some(StableScope.from(scope)) else None
    }
  }

  property("StableScope-Scope") = forAll { scope: StableScope =>
    scope.scope.withoutDefaultNamespace.isInvertible
  }

  property("defaultNamespaceOption") = forAll { scope: StableScope =>
    scope.defaultNamespaceOption == scope.scope.defaultNamespaceOption
  }

  property("keySet") = forAll { scope: StableScope =>
    scope.keySet == scope.scope.keySet
  }

  property("namespaces") = forAll { scope: StableScope =>
    scope.namespaces == scope.scope.namespaces
  }

  property("isCompatibleWith") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.isCompatibleWith(scope2) == {
      scope1.scope.doesNotConflictWith(scope2.scope) && (scope1.scope.defaultNamespaceOption.nonEmpty == scope2.scope.defaultNamespaceOption.nonEmpty)
    }
  }

  property("isCompatibleWith-2") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.isCompatibleWith(scope2) == {
      val commonPrefixes: Set[String] = scope1.keySet.intersect(scope2.keySet)
      scope1.scope.filterKeys(commonPrefixes) == scope2.scope.filterKeys(commonPrefixes) && (scope1.scope.defaultNamespaceOption.nonEmpty == scope2.scope.defaultNamespaceOption.nonEmpty)
    }
  }

  property("doesNotConflictWith") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.doesNotConflictWith(scope2) == {
      scope1.scope.doesNotConflictWith(scope2.scope)
    }
  }

  property("filterCompatibly") = forAll { scope: StableScope =>
    val p: ((String, String)) => Boolean = { _._1.length % 2 == 0 }

    scope.filterCompatibly(p) == StableScope
      .from(scope.scope.filter(p).append(scope.scope.retainingDefaultNamespace))
  }

  property("filterKeysCompatibly") = forAll { scope: StableScope =>
    val p: String => Boolean = { _.length % 2 == 0 }

    scope.filterKeysCompatibly(p) == StableScope
      .from(scope.scope.filterKeys(p).append(scope.scope.retainingDefaultNamespace))
  }

  property("filterNamespacesCompatibly") = forAll { scope: StableScope =>
    val p: String => Boolean = { _.length % 2 == 0 }

    scope.filterNamespacesCompatibly(p) == StableScope
      .from(scope.scope.filterNamespaces(p).append(scope.scope.retainingDefaultNamespace))
  }

  property("isCompatibleSubScopeOf") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.isCompatibleSubScopeOf(scope2) == {
      scope1.scope.subScopeOf(scope2.scope) && scope1.defaultNamespaceOption.nonEmpty == scope2.defaultNamespaceOption.nonEmpty
    }
  }

  property("isCompatibleSuperScopeOf") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.isCompatibleSuperScopeOf(scope2) == {
      scope2.isCompatibleSubScopeOf(scope1)
    }
  }

  property("canAppendCompatibleScope") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.canAppendCompatibleScope(scope2) == scope1.isCompatibleWith(scope2)
  }

  property("canAppendNonConflictingScope") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.canAppendNonConflictingScope(scope2) == {
      scope1.doesNotConflictWith(scope2) && scope2.scope.retainingDefaultNamespace.subScopeOf(scope1.scope.retainingDefaultNamespace)
    }
  }

  property("appendCompatibleScopeOption") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.appendCompatibleScopeOption(scope2) == {
      if (scope1.canAppendCompatibleScope(scope2)) {
        Some(StableScope.from(scope1.scope.append(scope2.scope)))
      } else {
        None
      }
    }
  }

  property("appendNonConflictingScopeOption") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.appendNonConflictingScopeOption(scope2) == {
      if (scope1.canAppendNonConflictingScope(scope2)) {
        Some(StableScope.from(scope1.scope.append(scope2.scope)))
      } else {
        None
      }
    }
  }

  // "Theorems"

  // A StableScope can be created if the Scope without default namespace is invertible

  property("optionallyFrom-defined") = forAll { scope: Scope =>
    StableScope.optionallyFrom(scope).nonEmpty == scope.withoutDefaultNamespace.isInvertible
  }

  // The result of "filtering compatibly" is a compatible sub-scope of the original scope

  property("filterCompatibly-isCompatibleSubScopeOf") = forAll { scope: StableScope =>
    val p: ((String, String)) => Boolean = { _._1.length % 2 == 0 }

    scope.filterCompatibly(p).isCompatibleSubScopeOf(scope)
  }

  property("filterKeysCompatibly-isCompatibleSubScopeOf") = forAll { scope: StableScope =>
    val p: String => Boolean = { _.length % 2 == 0 }

    scope.filterKeysCompatibly(p).isCompatibleSubScopeOf(scope)
  }

  property("filterNamespacesCompatibly-isCompatibleSubScopeOf") = forAll { scope: StableScope =>
    val p: String => Boolean = { _.length % 2 == 0 }

    scope.filterNamespacesCompatibly(p).isCompatibleSubScopeOf(scope)
  }

  // Function isCompatibleSubScopeOf means what the name says it does

  property("isCompatibleSubScopeOf-subScopeOf-isCompatibleWith") = forAll { scopePair: (StableScope, StableScope) =>
    val (scope1, scope2) = scopePair

    scope1.isCompatibleSubScopeOf(scope2) == {
      scope1.scope.subScopeOf(scope2.scope) && scope1.isCompatibleWith(scope2)
    }
  }

  property("appendCompatibleScope-appendNonConflictingScope") = forAll { scopePair: (StableScope, StableScope) =>
    scopePair._1.isCompatibleWith(scopePair._2) ==> {
      val (scope1, scope2) = scopePair

      scope1.appendCompatibleScope(scope2) == scope1.appendNonConflictingScope(scope2)
    }
  }

  property("appendCompatibleScope-isCompatibleSuperScopeOf") = forAll { scopePair: (StableScope, StableScope) =>
    scopePair._1.canAppendCompatibleScope(scopePair._2) ==> {
      val (scope1, scope2) = scopePair

      val combinedScope: StableScope = scope1.appendCompatibleScope(scope2)

      combinedScope.isCompatibleSuperScopeOf(scope1) &&
      combinedScope.isCompatibleSuperScopeOf(scope2)
    }
  }

  property("appendCompatibleScope-isCompatibleSuperScopeOf") = forAll { scopePair: (StableScope, StableScope) =>
    scopePair._1.canAppendNonConflictingScope(scopePair._2) ==> {
      val (scope1, scope2) = scopePair

      val combinedScope: StableScope = scope1.appendNonConflictingScope(scope2)

      combinedScope.isCompatibleSuperScopeOf(scope1) &&
      combinedScope.scope.superScopeOf(scope2.scope)
    }
  }

  // More interesting "theorems"

  // Two compatible sub-scopes of a superscope can be compatibly appended to each other and
  // the result is also a compatible sub-scope of the superscope.

  property("append-subscopes") = forAll { scopeTriple: StableScopeAndTwoCompatibleSubScopes =>
    val StableScopeAndTwoCompatibleSubScopes(superScope, scope1, scope2) = scopeTriple

    require(scope1.isCompatibleSubScopeOf(superScope))
    require(scope2.isCompatibleSubScopeOf(superScope))

    scope1.canAppendCompatibleScope(scope2) &&
    scope1.appendCompatibleScope(scope2).isCompatibleSubScopeOf(superScope)
  }

  // If a non-conflicting scope can be added to a scope, it can also be added to a compatible sub-scope of that scope,
  // resulting in a compatible sub-scope of the "new" superscope.

  property("append-to-subscope") = forAll { scopeTriple: StableScopeAndCompatibleSuperScopeAndExtraScope =>
    val StableScopeAndCompatibleSuperScopeAndExtraScope(StableScopeAndCompatibleSuperScope(scope, superScope), extraScope) = scopeTriple

    require(scope.isCompatibleSubScopeOf(superScope))
    require(superScope.canAppendNonConflictingScope(extraScope))

    scope.canAppendNonConflictingScope(extraScope) &&
    scope.appendNonConflictingScope(extraScope).isCompatibleSubScopeOf(superScope.appendNonConflictingScope(extraScope))
  }

  // Other properties

  property("isCompatibleWith-retainingDefaultNamespace") = forAll { scope: StableScope =>
    scope.isCompatibleWith(StableScope.from(scope.scope.retainingDefaultNamespace))
  }

  property("isCompatibleSuperScopeOf-retainingDefaultNamespace") = forAll { scope: StableScope =>
    scope.isCompatibleSuperScopeOf(StableScope.from(scope.scope.retainingDefaultNamespace))
  }
}

object StableScopeSpecification {

  final case class StableScopeAndTwoCompatibleSubScopes(superScope: StableScope, subScope1: StableScope, subScope2: StableScope) {
    require(subScope1.isCompatibleSubScopeOf(superScope))
    require(subScope2.isCompatibleSubScopeOf(superScope))
  }

  final case class StableScopeAndCompatibleSuperScope(scope: StableScope, superScope: StableScope) {
    require(scope.isCompatibleSubScopeOf(superScope))
  }

  final case class StableScopeAndCompatibleSuperScopeAndExtraScope(scopePair: StableScopeAndCompatibleSuperScope, extraScope: StableScope) {
    require(superScope.canAppendNonConflictingScope(extraScope))

    def scope: StableScope = scopePair.scope

    def superScope: StableScope = scopePair.superScope
  }
}
