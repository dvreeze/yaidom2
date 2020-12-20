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
import scala.util.chaining._

/**
 * "Creation DSL" element creation API, implementing ElemCreationApi.
 *
 * @author Chris de Vreeze
 */
final class NodeBuilderCreator(val knownStableScope: StableScope) extends ElemCreationApi {

  type WrapperType = NodeBuilders.ElemInKnownScope

  type NodeType = NodeBuilders.Node

  type ElemType = NodeBuilders.Elem

  def emptyElem(qname: QName): WrapperType = {
    emptyElem(qname, ListMap.empty, StableScope.empty)
  }

  def emptyElem(qname: QName, neededExtraStableScope: StableScope): WrapperType = {
    emptyElem(qname, ListMap.empty, neededExtraStableScope)
  }

  def emptyElem(qname: QName, attributesByQName: ListMap[QName, String]): WrapperType = {
    emptyElem(qname, attributesByQName, StableScope.empty)
  }

  def emptyElem(qname: QName, attributesByQName: ListMap[QName, String], neededExtraStableScope: StableScope): WrapperType = {
    elem(qname, attributesByQName, Vector.empty, neededExtraStableScope)
  }

  def textElem(qname: QName, txt: String): WrapperType = {
    textElem(qname, ListMap.empty, txt, StableScope.empty)
  }

  def textElem(qname: QName, txt: String, neededExtraStableScope: StableScope): WrapperType = {
    textElem(qname, ListMap.empty, txt, neededExtraStableScope)
  }

  def textElem(qname: QName, attributesByQName: ListMap[QName, String], txt: String): WrapperType = {
    textElem(qname, attributesByQName, txt, StableScope.empty)
  }

  def textElem(qname: QName, attributesByQName: ListMap[QName, String], txt: String, neededExtraStableScope: StableScope): WrapperType = {
    elem(qname, attributesByQName, Vector(Text(txt)), neededExtraStableScope)
  }

  def elem(qname: QName, children: Seq[NodeType]): WrapperType = {
    elem(qname, ListMap.empty, children, StableScope.empty)
  }

  def elem(qname: QName, children: Seq[NodeType], neededExtraStableScope: StableScope): WrapperType = {
    elem(qname, ListMap.empty, children, neededExtraStableScope)
  }

  def elem(qname: QName, attributesByQName: ListMap[QName, String], children: Seq[NodeType]): WrapperType = {
    elem(qname, attributesByQName, children, StableScope.empty)
  }

  /**
   * Creates an ElemInKnownScope whose element has the given QName, attributes (by QName) and child nodes.
   * That returned element has a stable scope that is a super-scope of neededExtraStableScope. The resulting knownStableScope
   * is also a super-scope of neededExtraStableScope, and is also a compatible super-scope of this NodeBuilderCreator's
   * knownStableScope. Of course, the returned ElemInKnownScope instance must also be internally consistent, in that
   * its element has a "combined stable scope" that is a compatible sub-scope of the ElemInKnownScope's knownStableScope.
   *
   * This method can be quite expensive if there are many children, or children with many descendant elements.
   *
   * The resulting element tree may have namespace undeclarations. Use method usingExtraScope to fix that.
   */
  def elem(
      qname: QName,
      attributesByQName: ListMap[QName, String],
      children: Seq[NodeType],
      neededExtraStableScope: StableScope): WrapperType = {
    val startKnownStableScope: StableScope = knownStableScope.appendNonConflictingScope(neededExtraStableScope)

    val childElems: Seq[ElemType] = children.collect { case e: NodeBuilders.Elem => e }
    val scopesOfDescendants: Seq[StableScope] = childElems.flatMap(_.findAllDescendantElemsOrSelf).map(_.stableScope).distinct

    // The code below throws if appending compatibly fails. Note that newKnownStableScope is a compatible super-scope of knownStableScope.
    // It is also a super-scope of neededExtraStableScope. It is also a compatible super-scope of the "combined stable scope"
    // of the returned element.

    val newKnownStableScope: StableScope = scopesOfDescendants.foldLeft(startKnownStableScope) {
      case (accKnownScope, currScope) =>
        accKnownScope.appendCompatibleScope(currScope)
    }

    // For element name and attributes, startKnownStableScope should be enough context passed to function minimizeStableScope.
    // Note that targetScope does not necessarily "contain" the descendant element scopes. Also note that targetScope is a
    // compatible sub-scope of newKnownStableScope, and so is the "combined stable scope" of the returned element.

    val targetScope: StableScope = ElemCreationApi
      .minimizeStableScope(startKnownStableScope, qname, attributesByQName.keySet)
      .appendNonConflictingScope(neededExtraStableScope)

    new NodeBuilders.Elem(qname, attributesByQName, targetScope, children.toVector)
      .pipe(e => ElemInKnownScope.from(e, newKnownStableScope)) // expensive, but it checks internal consistency
  }
}

object NodeBuilderCreator {

  def apply(knownStableScope: StableScope): NodeBuilderCreator = new NodeBuilderCreator(knownStableScope)
}
