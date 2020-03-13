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

import eu.cdevreeze.yaidom2.core.{EName, PrefixedScope, QName, Scope}
import eu.cdevreeze.yaidom2.node.DefaultUpdatableElemApiSpecificationDataProvider
import eu.cdevreeze.yaidom2.node.simple
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.updateapi.propertytests.UpdatableElemApiSpecification

import scala.collection.immutable.SeqMap

class SimpleElemUpdateApiSpecification
  extends DefaultUpdatableElemApiSpecificationDataProvider[simple.Node, simple.Elem]("Simple-UpdatableElemApi")
    with UpdatableElemApiSpecification[simple.Node, simple.Elem] {

  protected def convertSaxonElemToElem(e: saxon.Elem): simple.Elem = {
    simple.Elem.from(e)
  }

  protected def updateElem(e: simple.Elem): simple.Elem = {
    e.withAttributesByQName(e.attributesByQName + (QName.fromLocalName("testAttribute") -> "test"))
      .withChildren(e.children.appended(
        new simple.Elem(getQName(EName.fromLocalName("test-element"), e.scope), SeqMap.empty, e.scope, Vector(simple.Text("test", false)))))
  }

  protected def updateElemToNodeSeq(e: simple.Elem): Seq[simple.Node] = {
    Seq(
      e.withAttributesByQName(e.attributesByQName + (QName.fromLocalName("testAttribute") -> "test"))
        .withChildren(e.children.appended(
          new simple.Elem(getQName(EName.fromLocalName("test-element"), e.scope), SeqMap.empty, e.scope, Vector(simple.Text("test", false))))),
      simple.Text("addedText", false),
      new simple.Elem(getQName(EName.fromLocalName("other-test-element"), e.scope), SeqMap.empty, e.scope, Vector(simple.Text( "test2", false))))
  }

  private def getQName(ename: EName, scope: Scope): QName = {
    // Not complete. Does not recognize prefix "xml" .
    if (ename.namespaceUriOption.isEmpty) {
      require(scope.defaultNamespaceOption.isEmpty)
      QName.fromLocalName(ename.localPart)
    } else {
      PrefixedScope(scope.withoutDefaultNamespace).findQName(ename).getOrElse {
        require(scope.defaultNamespaceOption.contains(ename.namespaceUriOption.get))
        QName.fromLocalName(ename.localPart)}
    }
  }
}
