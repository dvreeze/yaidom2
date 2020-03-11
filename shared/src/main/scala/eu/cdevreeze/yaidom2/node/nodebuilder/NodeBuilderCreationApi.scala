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
import eu.cdevreeze.yaidom2.core.SimpleScope
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi
import eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Text

/**
 * "Creation DSL" element creation API. The passed NamespacePrefixMapper should be a total function from namespaces to prefixes.
 *
 * @author Chris de Vreeze
 */
final class NodeBuilderCreationApi(val namespacePrefixMapper: NamespacePrefixMapper) extends ElemCreationApi {

  // TODO Add knowledge about used namespaces (in attribute values and element text)

  type NodeType = NodeBuilders.Node

  type ElemType = NodeBuilders.Elem

  def emptyElem(name: EName, parentScope: SimpleScope): ElemType = {
    emptyElem(name, SeqMap.empty, parentScope)
  }

  def emptyElem(name: EName, attributes: SeqMap[EName, String], parentScope: SimpleScope): ElemType = {
    val scope: SimpleScope = parentScope.append(extractScope(attributes.keySet.incl(name)))
    new Elem(name, attributes, Vector.empty, scope)
  }

  def textElem(name: EName, txt: String, parentScope: SimpleScope): ElemType = {
    textElem(name, SeqMap.empty, txt, parentScope)
  }

  def textElem(name: EName, attributes: SeqMap[EName, String], txt: String, parentScope: SimpleScope): ElemType = {
    val scope: SimpleScope = parentScope.append(extractScope(attributes.keySet.incl(name)))
    new Elem(name, attributes, Vector(Text(txt)), scope)
  }

  def children(elem: ElemType): Seq[NodeType] = elem.children

  def withChildren(elem: ElemType, newChildren: Seq[NodeType]): ElemType = {
    new Elem(
      elem.name,
      elem.attributes,
      newChildren.map(ch => nodeUsingParentScope(ch, elem.simpleScope)).to(Vector),
      elem.simpleScope)
  }

  def plusChild(elem: ElemType, child: NodeType): ElemType = {
    new Elem(
      elem.name,
      elem.attributes,
      elem.children.appended(nodeUsingParentScope(child, elem.simpleScope)),
      elem.simpleScope)
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
      elem.children.appendedAll(childSeq.map(ch => nodeUsingParentScope(ch, elem.simpleScope))),
      elem.simpleScope)
  }

  def minusChild(elem: ElemType, index: Int): ElemType = {
    // Not efficient
    withChildren(elem, children(elem).patch(index, Seq.empty, 1))
  }

  def withAttributes(elem: ElemType, newAttributes: SeqMap[EName, String]): ElemType = {
    val scope: SimpleScope = elem.simpleScope.append(extractScope(newAttributes.keySet))
    new Elem(elem.name, newAttributes, elem.children, scope)
  }

  def plusAttribute(elem: ElemType, attrName: EName, attrValue: String): ElemType = {
    val scope: SimpleScope = elem.simpleScope.append(extractScope(attrName))
    new Elem(elem.name, elem.attributes.updated(attrName, attrValue), elem.children, scope)
  }

  def plusAttributeOption(elem: ElemType, attrName: EName, attrValueOption: Option[String]): ElemType = {
    plusAttributes(elem, attrValueOption.toSeq.map(v => attrName -> v).to(SeqMap))
  }

  def plusAttributes(elem: ElemType, newAttributes: SeqMap[EName, String]): ElemType = {
    val scope: SimpleScope = elem.simpleScope.append(extractScope(newAttributes.keySet))
    new Elem(elem.name, elem.attributes.concat(newAttributes), elem.children, scope)
  }

  def minusAttribute(elem: ElemType, attrName: EName): ElemType = {
    new Elem(elem.name, elem.attributes.removed(attrName), elem.children, elem.simpleScope)
  }

  def usingParentScope(elem: ElemType, parentScope: SimpleScope): ElemType = {
    val currentScope: SimpleScope = parentScope.append(elem.simpleScope)
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

  def nodeUsingParentScope(node: NodeType, parentScope: SimpleScope): NodeType = {
    node match {
      case e: Elem => usingParentScope(e, parentScope)
      case n => n
    }
  }

  def extractScope(ename: EName): SimpleScope = {
    extractScope(Set(ename))
  }

  def extractScope(enames: Set[EName]): SimpleScope = {
    val namespaces: Set[String] = enames.flatMap(_.namespaceUriOption)

    val prefixNamespaceMap: Map[String, String] = namespaces.toSeq.map { ns =>
      val prefix = namespacePrefixMapper.getPrefix(ns) // Throws if no prefix found!
      prefix -> ns
    }.toMap

    SimpleScope.from(prefixNamespaceMap)
  }
}

object NodeBuilderCreationApi {

  def apply(namespacePrefixMapper: NamespacePrefixMapper): NodeBuilderCreationApi = {
    new NodeBuilderCreationApi(namespacePrefixMapper)
  }

  implicit class WithCreationApi(val underlyingElem: NodeBuilders.Elem) {

    def creationApi(implicit nodeBuilderCreationApi: NodeBuilderCreationApi): Elem = {
      new Elem(underlyingElem)(nodeBuilderCreationApi)
    }
  }

  final class Elem(
    val underlyingElem: NodeBuilders.Elem)
    (implicit val nodeBuilderCreationApi: NodeBuilderCreationApi) extends ElemCreationApi.Elem {

    type UnderlyingNode = NodeBuilders.Node

    type UnderlyingElem = NodeBuilders.Elem

    type ThisElem = Elem

    def withChildren(newChildren: Seq[UnderlyingNode]): ThisElem = {
      nodeBuilderCreationApi.withChildren(underlyingElem, newChildren).pipe(wrap)
    }

    def plusChild(child: UnderlyingNode): ThisElem = {
      nodeBuilderCreationApi.plusChild(underlyingElem, child).pipe(wrap)
    }

    def plusChildOption(childOption: Option[UnderlyingNode]): ThisElem = {
      nodeBuilderCreationApi.plusChildOption(underlyingElem, childOption).pipe(wrap)
    }

    def plusChild(index: Int, child: UnderlyingNode): ThisElem = {
      nodeBuilderCreationApi.plusChild(underlyingElem, index, child).pipe(wrap)
    }

    def plusChildOption(index: Int, childOption: Option[UnderlyingNode]): ThisElem = {
      nodeBuilderCreationApi.plusChildOption(underlyingElem, index, childOption).pipe(wrap)
    }

    def plusChildren(childSeq: Seq[UnderlyingNode]): ThisElem = {
      nodeBuilderCreationApi.plusChildren(underlyingElem, childSeq).pipe(wrap)
    }

    def minusChild(index: Int): ThisElem = {
      nodeBuilderCreationApi.minusChild(underlyingElem, index).pipe(wrap)
    }

    def withAttributes(newAttributes: SeqMap[EName, String]): ThisElem = {
      nodeBuilderCreationApi.withAttributes(underlyingElem, newAttributes).pipe(wrap)
    }

    def plusAttribute(attrName: EName, attrValue: String): ThisElem = {
      nodeBuilderCreationApi.plusAttribute(underlyingElem, attrName, attrValue).pipe(wrap)
    }

    def plusAttributeOption(attrName: EName, attrValueOption: Option[String]): ThisElem = {
      nodeBuilderCreationApi.plusAttributeOption(underlyingElem, attrName, attrValueOption).pipe(wrap)
    }

    def plusAttributes(newAttributes: SeqMap[EName, String]): ThisElem = {
      nodeBuilderCreationApi.plusAttributes(underlyingElem, newAttributes).pipe(wrap)
    }

    def minusAttribute(attrName: EName): ThisElem = {
      nodeBuilderCreationApi.minusAttribute(underlyingElem, attrName).pipe(wrap)
    }

    def usingParentScope(parentScope: SimpleScope): ThisElem = {
      nodeBuilderCreationApi.usingParentScope(underlyingElem, parentScope).pipe(wrap)
    }

    def underlying: UnderlyingElem = underlyingElem

    private def wrap(underlyingElem: UnderlyingElem): ThisElem = {
      new Elem(underlyingElem)(nodeBuilderCreationApi)
    }
  }
}
