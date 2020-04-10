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

import scala.collection.immutable.SeqMap
import scala.util.chaining._

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.NamespacePrefixMapper
import eu.cdevreeze.yaidom2.core.PrefixedScope
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi
import eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Text

/**
 * "Creation DSL" element creation API. The passed NamespacePrefixMapper should be a total function from namespaces to prefixes.
 *
 * @author Chris de Vreeze
 */
final class NodeBuilderCreator(val namespacePrefixMapper: NamespacePrefixMapper) extends ElemCreationApi {

  // TODO Methods extractScope should also take a parentScope (as PrefixedScope)! Also for simple elements.

  // TODO Add knowledge about used namespaces (in attribute values and element text)

  type NodeType = NodeBuilders.Node

  type ElemType = NodeBuilders.Elem

  def emptyElem(name: EName, parentScope: PrefixedScope): ElemType = {
    emptyElem(name, SeqMap.empty, parentScope)
  }

  def emptyElem(name: EName, attributes: SeqMap[EName, String], parentScope: PrefixedScope): ElemType = {
    val extraScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq)
    require(parentScope.doesNotConflictWith(extraScope), s"Conflicting scopes '$parentScope' and '$extraScope'")
    val scope: PrefixedScope = parentScope.append(extraScope)

    new Elem(name, attributes, Vector.empty, scope)
  }

  def textElem(name: EName, txt: String, parentScope: PrefixedScope): ElemType = {
    textElem(name, SeqMap.empty, txt, parentScope)
  }

  def textElem(name: EName, attributes: SeqMap[EName, String], txt: String, parentScope: PrefixedScope): ElemType = {
    val extraScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq)
    require(parentScope.doesNotConflictWith(extraScope), s"Conflicting scopes '$parentScope' and '$extraScope'")
    val scope: PrefixedScope = parentScope.append(extraScope)

    new Elem(name, attributes, Vector(Text(txt)), scope)
  }

  def elem(name: EName, children: Seq[NodeType], parentScope: PrefixedScope): ElemType = {
    elem(name, SeqMap.empty, children, parentScope)
  }

  def elem(name: EName, attributes: SeqMap[EName, String], children: Seq[NodeType], parentScope: PrefixedScope): ElemType = {
    val extraScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq)
    require(parentScope.doesNotConflictWith(extraScope), s"Conflicting scopes '$parentScope' and '$extraScope'")
    val scope: PrefixedScope = parentScope.append(extraScope)

    new Elem(
      name,
      attributes,
      children.map(ch => nodeUsingNonConflictingParentScope(ch, scope)).to(Vector),
      scope)
  }

  def children(elem: ElemType): Seq[NodeType] = elem.children

  def withChildren(elem: ElemType, newChildren: Seq[NodeType]): ElemType = {
    new Elem(
      elem.name,
      elem.attributes,
      newChildren.map(ch => nodeUsingNonConflictingParentScope(ch, elem.prefixedScope)).to(Vector),
      elem.prefixedScope)
  }

  def plusChild(elem: ElemType, child: NodeType): ElemType = {
    new Elem(
      elem.name,
      elem.attributes,
      elem.children.appended(nodeUsingNonConflictingParentScope(child, elem.prefixedScope)),
      elem.prefixedScope)
  }

  def plusChildOption(elem: ElemType, childOption: Option[NodeType]): ElemType = {
    plusChildren(elem, childOption.toSeq)
  }

  def plusChild(elem: ElemType, index: Int, child: NodeType): ElemType = {
    // Not efficient
    withChildren(elem, children(elem).patch(index, Seq(child), 0))
  }

  def plusChildOption(elem: ElemType, index: Int, childOption: Option[NodeType]): ElemType = {
    // Not efficient
    withChildren(elem, children(elem).patch(index, childOption.toSeq, 0))
  }

  def plusChildren(elem: ElemType, childSeq: Seq[NodeType]): ElemType = {
    new Elem(
      elem.name,
      elem.attributes,
      elem.children.appendedAll(childSeq.map(ch => nodeUsingNonConflictingParentScope(ch, elem.prefixedScope))),
      elem.prefixedScope)
  }

  def minusChild(elem: ElemType, index: Int): ElemType = {
    // Not efficient
    withChildren(elem, children(elem).patch(index, Seq.empty, 1))
  }

  def withAttributes(elem: ElemType, newAttributes: SeqMap[EName, String]): ElemType = {
    val extraScope: PrefixedScope = extractScope(newAttributes.keys.toSeq)
    require(elem.prefixedScope.doesNotConflictWith(extraScope), s"Conflicting scopes '${elem.prefixedScope}' and '$extraScope'")
    val scope: PrefixedScope = elem.prefixedScope.append(extraScope)

    new Elem(elem.name, newAttributes, elem.children, scope)
  }

  def plusAttribute(elem: ElemType, attrName: EName, attrValue: String): ElemType = {
    val extraScope: PrefixedScope = extractScope(attrName)
    require(elem.prefixedScope.doesNotConflictWith(extraScope), s"Conflicting scopes '${elem.prefixedScope}' and '$extraScope'")
    val scope: PrefixedScope = elem.prefixedScope.append(extraScope)

    new Elem(elem.name, elem.attributes.updated(attrName, attrValue), elem.children, scope)
  }

  def plusAttributeOption(elem: ElemType, attrName: EName, attrValueOption: Option[String]): ElemType = {
    plusAttributes(elem, attrValueOption.toSeq.map(v => attrName -> v).to(SeqMap))
  }

  def plusAttributes(elem: ElemType, newAttributes: SeqMap[EName, String]): ElemType = {
    val extraScope: PrefixedScope = extractScope(newAttributes.keys.toSeq)
    require(elem.prefixedScope.doesNotConflictWith(extraScope), s"Conflicting scopes '${elem.prefixedScope}' and '$extraScope'")
    val scope: PrefixedScope = elem.prefixedScope.append(extraScope)

    new Elem(elem.name, elem.attributes.concat(newAttributes), elem.children, scope)
  }

  def minusAttribute(elem: ElemType, attrName: EName): ElemType = {
    new Elem(elem.name, elem.attributes.removed(attrName), elem.children, elem.prefixedScope)
  }

  def usingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType = {
    val currentScope: PrefixedScope = parentScope.append(elem.prefixedScope)
    // Recursive calls
    new Elem(
      elem.name,
      elem.attributes,
      elem.children.map {
        case che: Elem => usingParentScope(che, currentScope)
        case n => n
      },
      currentScope)
  }

  def nodeUsingParentScope(node: NodeType, parentScope: PrefixedScope): NodeType = {
    node match {
      case e: Elem => usingParentScope(e, parentScope)
      case n => n
    }
  }

  def usingNonConflictingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType = {
    require(parentScope.doesNotConflictWith(elem.prefixedScope), s"Conflicting scopes '$parentScope' and '${elem.prefixedScope}'")
    val currentScope: PrefixedScope = parentScope.append(elem.prefixedScope)
    // Recursive calls
    new Elem(
      elem.name,
      elem.attributes,
      elem.children.map {
        case che: Elem =>
          usingNonConflictingParentScope(che, currentScope)
        case n => n
      },
      currentScope)
  }

  def nodeUsingNonConflictingParentScope(node: NodeType, parentScope: PrefixedScope): NodeType = {
    node match {
      case e: Elem => usingNonConflictingParentScope(e, parentScope)
      case n => n
    }
  }

  def extractScope(ename: EName): PrefixedScope = {
    extractScope(Seq(ename))
  }

  def extractScope(enames: Seq[EName]): PrefixedScope = {
    val namespaces: Seq[String] = enames.flatMap(_.namespaceUriOption).distinct

    val prefixNamespaceMap: SeqMap[String, String] = namespaces.map { ns =>
      val prefix = namespacePrefixMapper.getPrefix(ns) // Throws if no prefix found!
      prefix -> ns
    }.to(SeqMap)

    PrefixedScope.from(prefixNamespaceMap)
  }
}

object NodeBuilderCreator {

  def apply(namespacePrefixMapper: NamespacePrefixMapper): NodeBuilderCreator = {
    new NodeBuilderCreator(namespacePrefixMapper)
  }

  implicit class WithCreationApi(val underlyingElem: NodeBuilders.Elem) {

    def creationApi(implicit nodeBuilderCreator: NodeBuilderCreator): Elem = {
      new Elem(underlyingElem)(nodeBuilderCreator)
    }
  }

  final class Elem(
    val underlyingElem: NodeBuilders.Elem)
    (implicit val nodeBuilderCreator: NodeBuilderCreator) extends ElemCreationApi.Elem {

    type UnderlyingNode = NodeBuilders.Node

    type UnderlyingElem = NodeBuilders.Elem

    type ThisElem = Elem

    def withChildren(newChildren: Seq[UnderlyingNode]): ThisElem = {
      nodeBuilderCreator.withChildren(underlyingElem, newChildren).pipe(wrap)
    }

    def plusChild(child: UnderlyingNode): ThisElem = {
      nodeBuilderCreator.plusChild(underlyingElem, child).pipe(wrap)
    }

    def plusChildOption(childOption: Option[UnderlyingNode]): ThisElem = {
      nodeBuilderCreator.plusChildOption(underlyingElem, childOption).pipe(wrap)
    }

    def plusChild(index: Int, child: UnderlyingNode): ThisElem = {
      nodeBuilderCreator.plusChild(underlyingElem, index, child).pipe(wrap)
    }

    def plusChildOption(index: Int, childOption: Option[UnderlyingNode]): ThisElem = {
      nodeBuilderCreator.plusChildOption(underlyingElem, index, childOption).pipe(wrap)
    }

    def plusChildren(childSeq: Seq[UnderlyingNode]): ThisElem = {
      nodeBuilderCreator.plusChildren(underlyingElem, childSeq).pipe(wrap)
    }

    def minusChild(index: Int): ThisElem = {
      nodeBuilderCreator.minusChild(underlyingElem, index).pipe(wrap)
    }

    def withAttributes(newAttributes: SeqMap[EName, String]): ThisElem = {
      nodeBuilderCreator.withAttributes(underlyingElem, newAttributes).pipe(wrap)
    }

    def plusAttribute(attrName: EName, attrValue: String): ThisElem = {
      nodeBuilderCreator.plusAttribute(underlyingElem, attrName, attrValue).pipe(wrap)
    }

    def plusAttributeOption(attrName: EName, attrValueOption: Option[String]): ThisElem = {
      nodeBuilderCreator.plusAttributeOption(underlyingElem, attrName, attrValueOption).pipe(wrap)
    }

    def plusAttributes(newAttributes: SeqMap[EName, String]): ThisElem = {
      nodeBuilderCreator.plusAttributes(underlyingElem, newAttributes).pipe(wrap)
    }

    def minusAttribute(attrName: EName): ThisElem = {
      nodeBuilderCreator.minusAttribute(underlyingElem, attrName).pipe(wrap)
    }

    def usingParentScope(parentScope: PrefixedScope): ThisElem = {
      nodeBuilderCreator.usingParentScope(underlyingElem, parentScope).pipe(wrap)
    }

    def usingNonConflictingParentScope(parentScope: PrefixedScope): ThisElem = {
      nodeBuilderCreator.usingNonConflictingParentScope(underlyingElem, parentScope).pipe(wrap)
    }

    def underlying: UnderlyingElem = underlyingElem

    private def wrap(underlyingElem: UnderlyingElem): ThisElem = {
      new Elem(underlyingElem)(nodeBuilderCreator)
    }
  }
}
