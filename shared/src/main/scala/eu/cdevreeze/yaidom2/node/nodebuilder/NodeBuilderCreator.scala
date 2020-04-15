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

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.NamespacePrefixMapper
import eu.cdevreeze.yaidom2.core.PrefixedScope
import eu.cdevreeze.yaidom2.core.PrefixedScopeUtil
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi
import eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Text

import scala.collection.immutable.ListMap
import scala.util.chaining._

/**
 * "Creation DSL" element creation API. The passed NamespacePrefixMapper should be a total function from namespaces to prefixes.
 *
 * The more the NamespacePrefixMapper and parent scopes (in several functions below) agree on namespace-prefix mappings,
 * the less chance there is that conflicts (prefixes mapping to different namespaces) occur, and the fewer corresponding
 * exceptions will be thrown by these functions. We should strive to work as much as possible with element trees that
 * contain no mutually conflicting scopes anywhere.
 *
 * @author Chris de Vreeze
 */
final class NodeBuilderCreator(val namespacePrefixMapper: NamespacePrefixMapper) extends ElemCreationApi {

  // TODO Add knowledge about used namespaces (in attribute values and element text)

  type NodeType = NodeBuilders.Node

  type ElemType = NodeBuilders.Elem

  private val prefixedScopeUtil = new PrefixedScopeUtil(namespacePrefixMapper)
  import prefixedScopeUtil.extractScope

  def emptyElem(name: EName, parentScope: PrefixedScope): ElemType = {
    emptyElem(name, ListMap.empty, parentScope)
  }

  def emptyElem(name: EName, attributes: ListMap[EName, String], parentScope: PrefixedScope): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq, parentScope)
    require(parentScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '$parentScope' and '$enhancedScope'")

    new Elem(name, attributes, Vector.empty, enhancedScope)
  }

  def textElem(name: EName, txt: String, parentScope: PrefixedScope): ElemType = {
    textElem(name, ListMap.empty, txt, parentScope)
  }

  def textElem(name: EName, attributes: ListMap[EName, String], txt: String, parentScope: PrefixedScope): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq, parentScope)
    require(parentScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '$parentScope' and '$enhancedScope'")

    new Elem(name, attributes, Vector(Text(txt)), enhancedScope)
  }

  def elem(name: EName, children: Seq[NodeType], parentScope: PrefixedScope): ElemType = {
    elem(name, ListMap.empty, children, parentScope)
  }

  def elem(name: EName, attributes: ListMap[EName, String], children: Seq[NodeType], parentScope: PrefixedScope): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq, parentScope)
    require(parentScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '$parentScope' and '$enhancedScope'")

    new Elem(name, attributes, children.map(ch => nodeUsingNonConflictingParentScope(ch, enhancedScope)).to(Vector), enhancedScope)
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

  def withAttributes(elem: ElemType, newAttributes: ListMap[EName, String]): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(newAttributes.keys.toSeq, elem.prefixedScope)
    require(elem.prefixedScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '${elem.prefixedScope}' and '$enhancedScope'")

    new Elem(elem.name, newAttributes, elem.children, enhancedScope)
  }

  def plusAttribute(elem: ElemType, attrName: EName, attrValue: String): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(attrName, elem.prefixedScope)
    require(elem.prefixedScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '${elem.prefixedScope}' and '$enhancedScope'")

    new Elem(elem.name, elem.attributes.updated(attrName, attrValue), elem.children, enhancedScope)
  }

  def plusAttributeOption(elem: ElemType, attrName: EName, attrValueOption: Option[String]): ElemType = {
    plusAttributes(elem, attrValueOption.toSeq.map(v => attrName -> v).to(ListMap))
  }

  def plusAttributes(elem: ElemType, newAttributes: ListMap[EName, String]): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(newAttributes.keys.toSeq, elem.prefixedScope)
    require(elem.prefixedScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '${elem.prefixedScope}' and '$enhancedScope'")

    new Elem(elem.name, elem.attributes.concat(newAttributes), elem.children, enhancedScope)
  }

  def minusAttribute(elem: ElemType, attrName: EName): ElemType = {
    new Elem(elem.name, elem.attributes.removed(attrName), elem.children, elem.prefixedScope)
  }

  def usingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType = {
    val currentScope: PrefixedScope = parentScope.append(elem.prefixedScope)
    // Recursive calls
    new Elem(elem.name, elem.attributes, elem.children.map {
      case che: Elem => usingParentScope(che, currentScope)
      case n         => n
    }, currentScope)
  }

  def nodeUsingParentScope(node: NodeType, parentScope: PrefixedScope): NodeType = {
    node match {
      case e: Elem => usingParentScope(e, parentScope)
      case n       => n
    }
  }

  def usingNonConflictingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType = {
    require(parentScope.doesNotConflictWith(elem.prefixedScope), s"Conflicting scopes '$parentScope' and '${elem.prefixedScope}'")
    val currentScope: PrefixedScope = parentScope.append(elem.prefixedScope)
    // Recursive calls
    new Elem(elem.name, elem.attributes, elem.children.map {
      case che: Elem => usingNonConflictingParentScope(che, currentScope)
      case n         => n
    }, currentScope)
  }

  def nodeUsingNonConflictingParentScope(node: NodeType, parentScope: PrefixedScope): NodeType = {
    node match {
      case e: Elem => usingNonConflictingParentScope(e, parentScope)
      case n       => n
    }
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

  final class Elem(val underlyingElem: NodeBuilders.Elem)(implicit val nodeBuilderCreator: NodeBuilderCreator)
      extends ElemCreationApi.Elem {

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

    def withAttributes(newAttributes: ListMap[EName, String]): ThisElem = {
      nodeBuilderCreator.withAttributes(underlyingElem, newAttributes).pipe(wrap)
    }

    def plusAttribute(attrName: EName, attrValue: String): ThisElem = {
      nodeBuilderCreator.plusAttribute(underlyingElem, attrName, attrValue).pipe(wrap)
    }

    def plusAttributeOption(attrName: EName, attrValueOption: Option[String]): ThisElem = {
      nodeBuilderCreator.plusAttributeOption(underlyingElem, attrName, attrValueOption).pipe(wrap)
    }

    def plusAttributes(newAttributes: ListMap[EName, String]): ThisElem = {
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

    // Taking the own PrefixedScope as parent scope

    def withChildrenFunction(newChildrenFunction: PrefixedScope => Seq[UnderlyingNode]): ThisElem = {
      withChildren(newChildrenFunction(prefixedScope))
    }

    def plusChildFunction(childFunction: PrefixedScope => UnderlyingNode): ThisElem = {
      plusChild(childFunction(prefixedScope))
    }

    def plusChildOptionFunction(childOptionFunction: PrefixedScope => Option[UnderlyingNode]): ThisElem = {
      plusChildOption(childOptionFunction(prefixedScope))
    }

    def plusChildFunction(index: Int, childFunction: PrefixedScope => UnderlyingNode): ThisElem = {
      plusChild(index, childFunction(prefixedScope))
    }

    def plusChildOptionFunction(index: Int, childOptionFunction: PrefixedScope => Option[UnderlyingNode]): ThisElem = {
      plusChildOption(index, childOptionFunction(prefixedScope))
    }

    def plusChildrenFunction(childSeqFunction: PrefixedScope => Seq[UnderlyingNode]): ThisElem = {
      plusChildren(childSeqFunction(prefixedScope))
    }

    def underlying: UnderlyingElem = underlyingElem

    def prefixedScope: PrefixedScope = underlying.prefixedScope

    private def wrap(underlyingElem: UnderlyingElem): ThisElem = {
      new Elem(underlyingElem)(nodeBuilderCreator)
    }
  }
}
