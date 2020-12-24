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

package eu.cdevreeze.yaidom2.node.nodebuilder

import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.StableScope
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi
import eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Text

import scala.collection.immutable.ListMap

/**
 * "Creation DSL" element creation API, implementing ElemCreationApi.
 *
 * @author Chris de Vreeze
 */
final class NodeBuilderCreator(val knownStableScope: StableScope) extends ElemCreationApi {

  type NodeType = NodeBuilders.Node

  type ElemType = NodeBuilders.Elem

  def emptyElem(qname: QName): ElemType = {
    emptyElem(qname, ListMap.empty, StableScope.empty)
  }

  def emptyElem(qname: QName, neededExtraStableScope: StableScope): ElemType = {
    emptyElem(qname, ListMap.empty, neededExtraStableScope)
  }

  def emptyElem(qname: QName, attributesByQName: ListMap[QName, String]): ElemType = {
    emptyElem(qname, attributesByQName, StableScope.empty)
  }

  def emptyElem(qname: QName, attributesByQName: ListMap[QName, String], neededExtraStableScope: StableScope): ElemType = {
    elem(qname, attributesByQName, Vector.empty, neededExtraStableScope)
  }

  def textElem(qname: QName, txt: String): ElemType = {
    textElem(qname, ListMap.empty, txt, StableScope.empty)
  }

  def textElem(qname: QName, txt: String, neededExtraStableScope: StableScope): ElemType = {
    textElem(qname, ListMap.empty, txt, neededExtraStableScope)
  }

  def textElem(qname: QName, attributesByQName: ListMap[QName, String], txt: String): ElemType = {
    textElem(qname, attributesByQName, txt, StableScope.empty)
  }

  def textElem(qname: QName, attributesByQName: ListMap[QName, String], txt: String, neededExtraStableScope: StableScope): ElemType = {
    elem(qname, attributesByQName, Vector(Text(txt)), neededExtraStableScope)
  }

  def elem(qname: QName, children: Seq[NodeType]): ElemType = {
    elem(qname, ListMap.empty, children, StableScope.empty)
  }

  def elem(qname: QName, children: Seq[NodeType], neededExtraStableScope: StableScope): ElemType = {
    elem(qname, ListMap.empty, children, neededExtraStableScope)
  }

  def elem(qname: QName, attributesByQName: ListMap[QName, String], children: Seq[NodeType]): ElemType = {
    elem(qname, attributesByQName, children, StableScope.empty)
  }

  /**
   * Creates an element that has the given QName, attributes (by QName) and child nodes.
   * That returned element has a stable scope that is a super-scope of neededExtraStableScope.
   *
   * This method can be expensive if there are many children.
   *
   * The resulting element tree may have namespace undeclarations. Use method usingExtraScopeDeeply or withoutNamespaceUndeclarations
   * to fix that.
   */
  def elem(
      qname: QName,
      attributesByQName: ListMap[QName, String],
      children: Seq[NodeType],
      neededExtraStableScope: StableScope): ElemType = {
    val startKnownStableScope: StableScope = knownStableScope.appendNonConflictingScope(neededExtraStableScope)

    // For element name and attributes, startKnownStableScope should be enough context passed to function minimizeStableScope.
    // Note that targetScope does not necessarily "contain" the descendant element scopes. Also note that targetScope is a
    // compatible sub-scope of newKnownStableScope, and so is the "combined stable scope" of the returned element.

    val targetScope: StableScope = ElemCreationApi
      .minimizeStableScope(startKnownStableScope, qname, attributesByQName.keySet)
      .appendNonConflictingScope(neededExtraStableScope)

    val elemWithoutChildren: Elem = new Elem(qname, attributesByQName, targetScope, Vector.empty, targetScope)

    elemWithoutChildren.withChildren(children)
  }
}

object NodeBuilderCreator {

  def apply(knownStableScope: StableScope): NodeBuilderCreator = new NodeBuilderCreator(knownStableScope)
}
