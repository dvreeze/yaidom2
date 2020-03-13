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

package eu.cdevreeze.yaidom2.node.simple.propertytests

import eu.cdevreeze.yaidom2.core.{EName, NamespacePrefixMapper, PrefixedScope, QName}
import eu.cdevreeze.yaidom2.node.DefaultUpdatableElemApiSpecificationDataProvider
import eu.cdevreeze.yaidom2.node.simple
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.simple.SimpleElemCreator
import eu.cdevreeze.yaidom2.updateapi.propertytests.UpdatableElemApiSpecification

class SimpleElemUpdateApiSpecification
  extends DefaultUpdatableElemApiSpecificationDataProvider[simple.Node, simple.Elem]("Simple-UpdatableElemApi")
    with UpdatableElemApiSpecification[simple.Node, simple.Elem] {

  protected def convertSaxonElemToElem(e: saxon.Elem): simple.Elem = {
    simple.Elem.from(e)
  }

  protected def updateElem(e: simple.Elem): simple.Elem = {
    val prefixedScope: PrefixedScope = PrefixedScope(e.scope.withoutDefaultNamespace)

    val mappings: Map[String, String] = prefixedScope.scope.prefixNamespaceMap.map(_.swap)

    val elemCreator: SimpleElemCreator = SimpleElemCreator(NamespacePrefixMapper.fromMapWithFallback(mappings))

    e.withAttributesByQName(e.attributesByQName + (QName.fromLocalName("testAttribute") -> "test"))
      .withChildren(e.children.appended(elemCreator.textElem(EName.fromLocalName("test-element"), "test", prefixedScope)))
  }

  protected def updateElemToNodeSeq(e: simple.Elem): Seq[simple.Node] = {
    val prefixedScope: PrefixedScope = PrefixedScope(e.scope.withoutDefaultNamespace)

    val mappings: Map[String, String] = prefixedScope.scope.prefixNamespaceMap.map(_.swap)

    val elemCreator: SimpleElemCreator = SimpleElemCreator(NamespacePrefixMapper.fromMapWithFallback(mappings))

    Seq(
      e.withAttributesByQName(e.attributesByQName + (QName.fromLocalName("testAttribute") -> "test"))
        .withChildren(e.children.appended(elemCreator.textElem(EName.fromLocalName("test-element"), "test", prefixedScope))),
      simple.Text("addedText", false),
      elemCreator.textElem(EName.fromLocalName("other-test-element"), "test2", prefixedScope))
  }
}
