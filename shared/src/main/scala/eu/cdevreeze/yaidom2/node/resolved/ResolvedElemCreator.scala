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
import eu.cdevreeze.yaidom2.core.PrefixedScope
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi

import scala.collection.immutable.ListMap
import scala.util.chaining._

/**
 * "Resolved" element creation API. The methods taking a parent scope ignore that parent scope.
 *
 * @author Chris de Vreeze
 */
object ResolvedElemCreator extends ElemCreationApi {

  type NodeType = ResolvedNodes.Node

  type ElemType = ResolvedNodes.Elem

  def emptyElem(name: EName, parentScope: PrefixedScope): ElemType = {
    emptyElem(name, ListMap.empty, parentScope)
  }

  def emptyElem(name: EName, attributes: ListMap[EName, String], parentScope: PrefixedScope): ElemType = {
    Elem(name, attributes, Vector.empty)
  }

  def textElem(name: EName, txt: String, parentScope: PrefixedScope): ElemType = {
    textElem(name, ListMap.empty, txt, parentScope)
  }

  def textElem(name: EName, attributes: ListMap[EName, String], txt: String, parentScope: PrefixedScope): ElemType = {
    Elem(name, attributes, Vector(Text(txt)))
  }

  def elem(name: EName, children: Seq[NodeType], parentScope: PrefixedScope): ElemType = {
    elem(name, ListMap.empty, children, parentScope)
  }

  def elem(name: EName, attributes: ListMap[EName, String], children: Seq[NodeType], parentScope: PrefixedScope): ElemType = {
    Elem(name, attributes, children.to(Vector))
  }

  def children(elem: ElemType): Seq[NodeType] = {
    elem.children
  }

  def withChildren(elem: ElemType, newChildren: Seq[NodeType]): ElemType = {
    Elem(elem.name, elem.attributes, newChildren.to(Vector))
  }

  def plusChild(elem: ElemType, child: NodeType): ElemType = {
    Elem(elem.name, elem.attributes, elem.children)
  }

  def plusChildOption(elem: ElemType, childOption: Option[NodeType]): ElemType = {
    plusChildren(elem, childOption.toSeq)
  }

  def plusChild(elem: ElemType, index: Int, child: NodeType): ElemType = {
    withChildren(elem, children(elem).patch(index, Seq(child), 0))
  }

  def plusChildOption(elem: ElemType, index: Int, childOption: Option[NodeType]): ElemType = {
    withChildren(elem, children(elem).patch(index, childOption.toSeq, 0))
  }

  def plusChildren(elem: ElemType, childSeq: Seq[NodeType]): ElemType = {
    Elem(elem.name, elem.attributes, elem.children.appendedAll(childSeq))
  }

  def minusChild(elem: ElemType, index: Int): ElemType = {
    withChildren(elem, children(elem).patch(index, Seq.empty, 1))
  }

  def withAttributes(elem: ElemType, newAttributes: ListMap[EName, String]): ElemType = {
    Elem(elem.name, newAttributes, elem.children)
  }

  def plusAttribute(elem: ElemType, attrName: EName, attrValue: String): ElemType = {
    Elem(elem.name, elem.attributes.updated(attrName, attrValue), elem.children)
  }

  def plusAttributeOption(elem: ElemType, attrName: EName, attrValueOption: Option[String]): ElemType = {
    plusAttributes(elem, attrValueOption.toSeq.map(v => attrName -> v).to(ListMap))
  }

  def plusAttributes(elem: ElemType, newAttributes: ListMap[EName, String]): ElemType = {
    Elem(elem.name, elem.attributes.concat(newAttributes), elem.children)
  }

  def minusAttribute(elem: ElemType, attrName: EName): ElemType = {
    Elem(elem.name, elem.attributes.removed(attrName), elem.children)
  }

  def withName(elem: ElemType, newName: EName): ElemType = {
    Elem(newName, elem.attributes, elem.children)
  }

  def usingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType = elem

  def usingNonConflictingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType = elem

  implicit class WithCreationApi(val underlyingElem: ResolvedNodes.Elem) {

    def creationApi(implicit elemCreator: ResolvedElemCreator.type): Elem = {
      new Elem(underlyingElem)(elemCreator)
    }
  }

  final class Elem(val underlyingElem: ResolvedNodes.Elem)(implicit val elemCreator: ResolvedElemCreator.type)
      extends ElemCreationApi.Elem {

    type UnderlyingNode = ResolvedNodes.Node

    type UnderlyingElem = ResolvedNodes.Elem

    type ThisElem = Elem

    def withChildren(newChildren: Seq[UnderlyingNode]): ThisElem = {
      elemCreator.withChildren(underlyingElem, newChildren).pipe(wrap)
    }

    def plusChild(child: UnderlyingNode): ThisElem = {
      elemCreator.plusChild(underlyingElem, child).pipe(wrap)
    }

    def plusChildOption(childOption: Option[UnderlyingNode]): ThisElem = {
      elemCreator.plusChildOption(underlyingElem, childOption).pipe(wrap)
    }

    def plusChild(index: Int, child: UnderlyingNode): ThisElem = {
      elemCreator.plusChild(underlyingElem, index, child).pipe(wrap)
    }

    def plusChildOption(index: Int, childOption: Option[UnderlyingNode]): ThisElem = {
      elemCreator.plusChildOption(underlyingElem, index, childOption).pipe(wrap)
    }

    def plusChildren(childSeq: Seq[UnderlyingNode]): ThisElem = {
      elemCreator.plusChildren(underlyingElem, childSeq).pipe(wrap)
    }

    def minusChild(index: Int): ThisElem = {
      elemCreator.minusChild(underlyingElem, index).pipe(wrap)
    }

    def withAttributes(newAttributes: ListMap[EName, String]): ThisElem = {
      elemCreator.withAttributes(underlyingElem, newAttributes).pipe(wrap)
    }

    def plusAttribute(attrName: EName, attrValue: String): ThisElem = {
      elemCreator.plusAttribute(underlyingElem, attrName, attrValue).pipe(wrap)
    }

    def plusAttributeOption(attrName: EName, attrValueOption: Option[String]): ThisElem = {
      elemCreator.plusAttributeOption(underlyingElem, attrName, attrValueOption).pipe(wrap)
    }

    def plusAttributes(newAttributes: ListMap[EName, String]): ThisElem = {
      elemCreator.plusAttributes(underlyingElem, newAttributes).pipe(wrap)
    }

    def minusAttribute(attrName: EName): ThisElem = {
      elemCreator.minusAttribute(underlyingElem, attrName).pipe(wrap)
    }

    def withName(newName: EName): ThisElem = {
      elemCreator.withName(underlyingElem, newName).pipe(wrap)
    }

    def usingParentScope(parentScope: PrefixedScope): ThisElem = {
      elemCreator.usingParentScope(underlyingElem, parentScope).pipe(wrap)
    }

    def usingNonConflictingParentScope(parentScope: PrefixedScope): ThisElem = {
      elemCreator.usingNonConflictingParentScope(underlyingElem, parentScope).pipe(wrap)
    }

    def underlying: UnderlyingElem = underlyingElem

    private def wrap(underlyingElem: UnderlyingElem): ThisElem = {
      new Elem(underlyingElem)(elemCreator)
    }
  }
}
