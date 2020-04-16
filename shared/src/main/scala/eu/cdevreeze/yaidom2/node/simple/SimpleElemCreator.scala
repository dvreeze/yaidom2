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

package eu.cdevreeze.yaidom2.node.simple

import eu.cdevreeze.yaidom2.core._
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi

import scala.collection.immutable.ListMap
import scala.util.chaining._

/**
 * "Simple" element creation API. The passed NamespacePrefixMapper should be a total function from namespaces to prefixes.
 *
 * The more the NamespacePrefixMapper and parent scopes (in several functions below) agree on namespace-prefix mappings,
 * the less chance there is that conflicts (prefixes mapping to different namespaces) occur, and the fewer corresponding
 * exceptions will be thrown by these functions. We should strive to work as much as possible with collections of element trees that
 * contain no mutually conflicting scopes anywhere, as per method ScopedElemApi.containsNoConflictingScopes.
 *
 * @author Chris de Vreeze
 */
final class SimpleElemCreator(val namespacePrefixMapper: NamespacePrefixMapper) extends ElemCreationApi {

  type NodeType = SimpleNodes.Node

  type ElemType = SimpleNodes.Elem

  private val prefixedScopeUtil = new PrefixedScopeUtil(namespacePrefixMapper)
  import prefixedScopeUtil.extractScope

  def emptyElem(name: EName, parentScope: PrefixedScope): ElemType = {
    emptyElem(name, ListMap.empty, parentScope)
  }

  def emptyElem(name: EName, attributes: ListMap[EName, String], parentScope: PrefixedScope): ElemType = {
    // Not using the default namespace
    val enhancedScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq, parentScope)
    require(parentScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '$parentScope' and '$enhancedScope'")

    new Elem(
      enhancedScope.getQName(name),
      attributes.map { case (ename, v) => enhancedScope.getQName(ename) -> v },
      enhancedScope.scope,
      Vector.empty)
  }

  def textElem(name: EName, txt: String, parentScope: PrefixedScope): ElemType = {
    textElem(name, ListMap.empty, txt, parentScope)
  }

  def textElem(name: EName, attributes: ListMap[EName, String], txt: String, parentScope: PrefixedScope): ElemType = {
    // Not using the default namespace
    val enhancedScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq, parentScope)
    require(parentScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '$parentScope' and '$enhancedScope'")

    new Elem(
      enhancedScope.getQName(name),
      attributes.map { case (ename, v) => enhancedScope.getQName(ename) -> v },
      enhancedScope.scope,
      Vector(Text(txt, isCData = false)))
  }

  def elem(name: EName, children: Seq[NodeType], parentScope: PrefixedScope): ElemType = {
    elem(name, ListMap.empty, children, parentScope)
  }

  def elem(name: EName, attributes: ListMap[EName, String], children: Seq[NodeType], parentScope: PrefixedScope): ElemType = {
    // Here we do have to take the default namespace of the children into account, if any (see overloaded nodeUsingNonConflictingParentScope method)
    val enhancedScope: PrefixedScope = extractScope(name +: attributes.keys.toIndexedSeq, parentScope)
    require(parentScope.doesNotConflictWith(enhancedScope), s"Conflicting scopes '$parentScope' and '$enhancedScope'")

    new Elem(
      enhancedScope.getQName(name),
      attributes.map { case (ename, v) => enhancedScope.getQName(ename) -> v },
      enhancedScope.scope,
      children.to(Vector).map {
        case che: Elem => nodeUsingNonConflictingParentScope(che, enhancedScope.scope)
        case ch: Node  => ch
      }
    )
  }

  def children(elem: ElemType): Seq[NodeType] = elem.children

  def withChildren(elem: ElemType, newChildren: Seq[NodeType]): ElemType = {
    new Elem(
      elem.qname,
      elem.attributesByQName,
      elem.scope,
      newChildren.map(ch => nodeUsingNonConflictingParentScope(ch, elem.scope)).to(Vector))
  }

  def plusChild(elem: ElemType, child: NodeType): ElemType = {
    new Elem(elem.qname, elem.attributesByQName, elem.scope, elem.children.appended(nodeUsingNonConflictingParentScope(child, elem.scope)))
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
      elem.qname,
      elem.attributesByQName,
      elem.scope,
      elem.children.appendedAll(childSeq.map(ch => nodeUsingNonConflictingParentScope(ch, elem.scope))))
  }

  def minusChild(elem: ElemType, index: Int): ElemType = {
    // Not efficient
    withChildren(elem, children(elem).patch(index, Seq.empty, 1))
  }

  def withAttributes(elem: ElemType, newAttributes: ListMap[EName, String]): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(newAttributes.keys.toSeq, PrefixedScope.from(elem.scope.withoutDefaultNamespace))
    require(elem.scope.doesNotConflictWith(enhancedScope.scope), s"Conflicting scopes '${elem.scope}' and '$enhancedScope'")
    val scope: Scope = elem.scope.append(enhancedScope.scope)
    val prefixedScope: PrefixedScope = PrefixedScope.from(scope.withoutDefaultNamespace)

    new Elem(elem.qname, newAttributes.map { case (ename, v) => prefixedScope.getQName(ename) -> v }, scope, elem.children)
  }

  def plusAttribute(elem: ElemType, attrName: EName, attrValue: String): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(attrName, PrefixedScope.from(elem.scope.withoutDefaultNamespace))
    require(elem.scope.doesNotConflictWith(enhancedScope.scope), s"Conflicting scopes '${elem.scope}' and '$enhancedScope'")
    val scope: Scope = elem.scope.append(enhancedScope.scope)
    val prefixedScope: PrefixedScope = PrefixedScope.from(scope.withoutDefaultNamespace)

    new Elem(
      elem.qname,
      elem.attributes.updated(attrName, attrValue).map { case (ename, v) => prefixedScope.getQName(ename) -> v },
      scope,
      elem.children)
  }

  def plusAttributeOption(elem: ElemType, attrName: EName, attrValueOption: Option[String]): ElemType = {
    plusAttributes(elem, attrValueOption.toSeq.map(v => attrName -> v).to(ListMap))
  }

  def plusAttributes(elem: ElemType, newAttributes: ListMap[EName, String]): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(newAttributes.keys.toSeq, PrefixedScope.from(elem.scope.withoutDefaultNamespace))
    require(elem.scope.doesNotConflictWith(enhancedScope.scope), s"Conflicting scopes '${elem.scope}' and '$enhancedScope'")
    val scope: Scope = elem.scope.append(enhancedScope.scope)
    val prefixedScope: PrefixedScope = PrefixedScope.from(scope.withoutDefaultNamespace)

    new Elem(
      elem.qname,
      elem.attributes.concat(newAttributes).map { case (ename, v) => prefixedScope.getQName(ename) -> v },
      scope,
      elem.children)
  }

  def minusAttribute(elem: ElemType, attrName: EName): ElemType = {
    val prefixedScope: PrefixedScope = PrefixedScope(elem.scope.withoutDefaultNamespace)

    new Elem(
      elem.qname,
      elem.attributes.removed(attrName).map { case (ename, v) => prefixedScope.getQName(ename) -> v },
      elem.scope,
      elem.children)
  }

  def withName(elem: ElemType, newName: EName): ElemType = {
    val enhancedScope: PrefixedScope = extractScope(newName, PrefixedScope.from(elem.scope.withoutDefaultNamespace))
    require(elem.scope.doesNotConflictWith(enhancedScope.scope), s"Conflicting scopes '${elem.scope}' and '$enhancedScope'")
    val scope: Scope = elem.scope.append(enhancedScope.scope)
    val prefixedScope: PrefixedScope = PrefixedScope.from(scope.withoutDefaultNamespace)

    new Elem(prefixedScope.getQName(newName), elem.attributesByQName, scope, elem.children)
  }

  def usingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType = {
    usingParentScope(elem, parentScope.scope)
  }

  def nodeUsingParentScope(node: NodeType, parentScope: PrefixedScope): NodeType = {
    nodeUsingParentScope(node, parentScope.scope)
  }

  def usingNonConflictingParentScope(elem: ElemType, parentScope: PrefixedScope): ElemType = {
    usingNonConflictingParentScope(elem, parentScope.scope)
  }

  def nodeUsingNonConflictingParentScope(node: NodeType, parentScope: PrefixedScope): NodeType = {
    nodeUsingNonConflictingParentScope(node, parentScope.scope)
  }

  def usingParentScope(elem: ElemType, parentScope: Scope): ElemType = {
    val currentScope: Scope = parentScope.append(elem.scope).ensuring(_.superScopeOf(elem.scope))
    // Recursive calls
    new Elem(elem.qname, elem.attributesByQName, currentScope, elem.children.map {
      case che: Elem => usingParentScope(che, currentScope)
      case n         => n
    })
  }

  def nodeUsingParentScope(node: NodeType, parentScope: Scope): NodeType = {
    node match {
      case e: Elem => usingParentScope(e, parentScope)
      case n       => n
    }
  }

  def usingNonConflictingParentScope(elem: ElemType, parentScope: Scope): ElemType = {
    require(parentScope.doesNotConflictWith(elem.scope), s"Conflicting scopes '$parentScope' and '${elem.scope}'")
    val currentScope: Scope = parentScope.append(elem.scope).ensuring(_.superScopeOf(elem.scope))
    // Recursive calls
    new Elem(elem.qname, elem.attributesByQName, currentScope, elem.children.map {
      case che: Elem => usingNonConflictingParentScope(che, currentScope)
      case n         => n
    })
  }

  def nodeUsingNonConflictingParentScope(node: NodeType, parentScope: Scope): NodeType = {
    node match {
      case e: Elem => usingNonConflictingParentScope(e, parentScope)
      case n       => n
    }
  }
}

object SimpleElemCreator {

  def apply(namespacePrefixMapper: NamespacePrefixMapper): SimpleElemCreator = {
    new SimpleElemCreator(namespacePrefixMapper)
  }

  implicit class WithCreationApi(val underlyingElem: SimpleNodes.Elem) {

    def creationApi(implicit elemCreator: SimpleElemCreator): Elem = {
      new Elem(underlyingElem)(elemCreator)
    }
  }

  final class Elem(val underlyingElem: SimpleNodes.Elem)(implicit val elemCreator: SimpleElemCreator) extends ElemCreationApi.Elem {

    type UnderlyingNode = SimpleNodes.Node

    type UnderlyingElem = SimpleNodes.Elem

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
