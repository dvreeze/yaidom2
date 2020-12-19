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
import org.scalacheck.Prop.forAll
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
    } yield Scope.from(prefixNamespaceMap.to(ListMap))
  }

  private val genStableScope: Gen[StableScope] = {
    for {
      sc <- genScope.suchThat(_.withoutDefaultNamespace.isInvertible)
    } yield StableScope.from(sc)
  }

  implicit val arbitraryScope: Arbitrary[Scope] = {
    Arbitrary(genScope)
  }

  implicit val arbitraryStableScope: Arbitrary[StableScope] = {
    Arbitrary(genStableScope)
  }

  property("optionallyFrom") = forAll { scope: Scope =>
    StableScope.optionallyFrom(scope).nonEmpty == (scope.withoutDefaultNamespace.isInvertible)
  }

  property("isCompatibleWith") = forAll { (scope1: StableScope, scope2: StableScope) =>
    scope1.isCompatibleWith(scope2) == {
      scope1.scope.doesNotConflictWith(scope2.scope) && (scope1.scope.defaultNamespaceOption.nonEmpty == scope2.scope.defaultNamespaceOption.nonEmpty)
    }
  }

  property("isCompatibleWith-2") = forAll { (scope1: StableScope, scope2: StableScope) =>
    scope1.isCompatibleWith(scope2) == {
      val commonPrefixes: Set[String] = scope1.keySet.intersect(scope2.keySet)
      scope1.scope.filterKeys(commonPrefixes) == scope2.scope.filterKeys(commonPrefixes) && (scope1.scope.defaultNamespaceOption.nonEmpty == scope2.scope.defaultNamespaceOption.nonEmpty)
    }
  }

  property("compatible-with-scope-retaining-default-namespace") = forAll { scope: StableScope =>
    scope.isCompatibleWith(StableScope.from(scope.scope.retainingDefaultNamespace))
  }

  property("compatible-super-scope-of-scope-retaining-default-namespace") = forAll { scope: StableScope =>
    scope.isCompatibleSuperScopeOf(StableScope.from(scope.scope.retainingDefaultNamespace))
  }
}
