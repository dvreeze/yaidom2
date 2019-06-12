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

package eu.cdevreeze.yaidom2.node.resolved.propertytests

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.DefaultUpdatableElemApiSpecificationDataProvider
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.updateapi.propertytests.UpdatableElemApiSpecification

class ResolvedElemUpdateApiSpecification
  extends DefaultUpdatableElemApiSpecificationDataProvider[resolved.Node, resolved.Elem]("Resolved-UpdatableElemApi")
    with UpdatableElemApiSpecification[resolved.Node, resolved.Elem] {

  protected def convertSaxonElemToElem(e: saxon.Elem): resolved.Elem = {
    resolved.Elem.from(e)
  }

  protected def updateElem(e: resolved.Elem): resolved.Elem = {
    e.copy(attributes = e.attributes + (EName.fromLocalName("testAttribute") -> "test"))
      .copy(children = e.children.appended(resolved.Node.textElem(EName.fromLocalName("test-element"), "test")))
  }

  protected def updateElemToNodeSeq(e: resolved.Elem): Seq[resolved.Node] = {
    Seq(
      e.copy(attributes = e.attributes + (EName.fromLocalName("testAttribute") -> "test"))
        .copy(children = e.children.appended(resolved.Node.textElem(EName.fromLocalName("test-element"), "test"))),
      resolved.Text("addedText"),
      resolved.Node.textElem(EName.fromLocalName("other-test-element"), "test2"))
  }
}
