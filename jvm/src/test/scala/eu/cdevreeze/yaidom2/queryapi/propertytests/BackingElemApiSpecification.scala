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

package eu.cdevreeze.yaidom2.queryapi.propertytests

import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Properties

trait BackingElemApiSpecification[N, E <: BackingNodes.Elem.Aux[N, E]] extends ScopedElemApiSpecification[N, E] {
  self: Properties =>

  property("parent-of-child-is-this") = forAll { elem: E =>
    (elem.filterChildElems(_ => true).nonEmpty) ==>
      (elem.filterChildElems(_ => true).flatMap(_.findParentElem(_ => true)).distinct == Seq(elem))
  }

  property("an-ancestor-of-descendant-is-this") = forAll { elem: E =>
    (elem.filterChildElems(_ => true).nonEmpty) ==>
      (elem.filterDescendantElems(_ => true).flatMap(_.findAncestorElem(_ == elem)).distinct == Seq(elem))
  }

  property("an-ancestor-or-self-of-descendant-or-self-is-this") = forAll { elem: E =>
    elem.filterDescendantElemsOrSelf(_ => true).flatMap(_.findAncestorElemOrSelf(_ == elem)).distinct == Seq(elem)
  }
}
