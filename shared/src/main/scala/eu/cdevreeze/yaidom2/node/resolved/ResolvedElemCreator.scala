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

package eu.cdevreeze.yaidom2.node.resolved

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.StableScope
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi

import scala.collection.immutable.ListMap

/**
 * "Resolved" element creation API.
 *
 * @author Chris de Vreeze
 */
final class ResolvedElemCreator(val knownStableScope: StableScope) extends ElemCreationApi {

  type NodeType = ResolvedNodes.Node

  type ElemType = ResolvedNodes.Elem

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

  def elem(
      qname: QName,
      attributesByQName: ListMap[QName, String],
      children: Seq[NodeType],
      neededExtraStableScope: StableScope): ElemType = {
    val newKnownStableScope: StableScope = knownStableScope.appendNonConflictingScope(neededExtraStableScope)

    val ename: EName = newKnownStableScope.resolveQName(qname)

    ResolvedNodes
      .Elem(ename, convertAttributes(attributesByQName, newKnownStableScope), children.toVector)
  }

  private def convertAttributes(attributesByQName: ListMap[QName, String], stableScope: StableScope): ListMap[EName, String] = {
    val attrScope = stableScope.scope.withoutDefaultNamespace

    attributesByQName.collect {
      case (attQName, attValue) =>
        val attEName = attrScope
          .resolveQNameOption(attQName)
          .getOrElse(sys.error(s"Attribute name '$attQName' should resolve to an EName in scope [$attrScope]"))

        attEName -> attValue
    }
  }
}

object ResolvedElemCreator {

  def apply(knownStableScope: StableScope): ResolvedElemCreator = new ResolvedElemCreator(knownStableScope)
}
