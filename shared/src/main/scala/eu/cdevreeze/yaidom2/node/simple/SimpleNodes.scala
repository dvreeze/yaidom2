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

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap
import scala.reflect.classTag

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.creationapi.ScopedNodeFactories
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractScopedElem
import eu.cdevreeze.yaidom2.updateapi.internal.AbstractUpdatableElem

/**
 * "Simple" nodes.
 *
 * @author Chris de Vreeze
 */
object SimpleNodes {

  // First the OO query API

  /**
   * Arbitrary simple node
   */
  sealed trait Node extends ScopedNodes.Node

  /**
   * Potential document child
   */
  sealed trait CanBeDocumentChild extends Node with ScopedNodes.CanBeDocumentChild

  /**
   * "Simple" element node, offering the `ScopedNodes.Elem` element query API.
   */
  final class Elem(
    val qname: QName,
    val attributesByQName: SeqMap[QName, String],
    val scope: Scope,
    val children: ArraySeq[Node]
  ) extends CanBeDocumentChild with AbstractScopedElem with AbstractUpdatableElem {

    // TODO Requirements on constructor parameters

    type ThisElem = Elem

    type ThisNode = Node

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      ArraySeq.from(xs)(classTag[Elem])
    }

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e@Elem(_, _, _, _) if p(e) => e }
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e@Elem(_, _, _, _) if p(e) => e }
    }

    def findDescendantElemOrSelf(navigationPath: Seq[Int]): Option[ThisElem] = {
      if (navigationPath.isEmpty) {
        Some(self)
      } else {
        val childElemIdx: Int = navigationPath(0)
        val childElems: Seq[Elem] = findAllChildElems()

        if (childElemIdx >= 0 && childElemIdx < childElems.size) {
          // Recursive call
          Option(childElems(childElemIdx)).flatMap(_.findDescendantElemOrSelf(navigationPath.drop(1)))
        } else {
          None
        }
      }
    }

    def name: EName = {
      scope.resolveQNameOption(qname)
        .getOrElse(sys.error(s"Element name '${qname}' should resolve to an EName in scope [${scope}]"))
    }

    def attributes: SeqMap[EName, String] = {
      val attrScope = attributeScope

      attributesByQName.map { kv =>
        val attQName = kv._1
        val attValue = kv._2
        val attEName = attrScope.resolveQNameOption(attQName)
          .getOrElse(sys.error(s"Attribute name '${attQName}' should resolve to an EName in scope [${attrScope}]"))

        (attEName -> attValue)
      }
    }

    // Update API methods

    def withChildren(newChildren: Seq[ThisNode]): ThisElem = {
      new Elem(qname, attributesByQName, scope, newChildren.to(ArraySeq))
    }

    protected def findAllChildElemsWithSteps: Seq[(ThisElem, Int)] = {
      findAllChildElems.zipWithIndex
    }

    // Transformation API methods

    def transformChildElems(f: ThisElem => ThisElem): ThisElem = {
      val resultChildNodes: ArraySeq[ThisNode] =
        children.map {
          case e: Elem => f(e)
          case n => n
        }

      withChildren(resultChildNodes)
    }

    def transformChildElemsToNodeSeq(f: ThisElem => Seq[ThisNode]): ThisElem = {
      val resultChildNodes: ArraySeq[ThisNode] =
        children.flatMap {
          case e: Elem => f(e)
          case n => ArraySeq(n)
        }

      withChildren(resultChildNodes)
    }

    // Other public methods

    def attributeScope: Scope = scope.withoutDefaultNamespace

    def withAttributesByQName(newAttributesByQName: SeqMap[QName, String]): ThisElem = {
      new Elem(qname, newAttributesByQName, scope, children)
    }
  }

  /**
   * "Simple" text node
   */
  final case class Text(text: String, isCData: Boolean) extends Node with ScopedNodes.Text

  /**
   * "Simple" comment node
   */
  final case class Comment(text: String) extends CanBeDocumentChild with ScopedNodes.Comment

  /**
   * "Simple" processing instruction
   */
  final case class ProcessingInstruction(target: String, data: String) extends CanBeDocumentChild with ScopedNodes.ProcessingInstruction

  // Next the functional query API
  // TODO ElemCreationApi (using invertible Scope as state)

  object Node extends ScopedNodeFactories.NodeFactory {

    type TargetNodeType = Node

    def from(node: ScopedNodes.Node): Node = node match {
      case e: ScopedNodes.Elem => Elem.from(e)
      case t: ScopedNodes.Text => Text(t.text, false)
      case c: ScopedNodes.Comment => Comment(c.text)
      case pi: ScopedNodes.ProcessingInstruction => ProcessingInstruction(pi.target, pi.data)
      case n => sys.error(s"Not an element, text, comment or processing instruction node: $n")
    }
  }

  object CanBeDocumentChild {

    def from(node: ScopedNodes.CanBeDocumentChild): CanBeDocumentChild = node match {
      case e: ScopedNodes.Elem => Elem.from(e)
      case c: ScopedNodes.Comment => Comment(c.text)
      case pi: ScopedNodes.ProcessingInstruction => ProcessingInstruction(pi.target, pi.data)
      case n => sys.error(s"Not an element, comment or processing instruction node: $n")
    }
  }

  object Elem extends ScopedNodeFactories.ElemFactory {

    type ElemType = Elem

    type NodeType = Node

    type TargetElemType = Elem

    def unapply(elem: Elem): Option[(QName, SeqMap[QName, String], Scope, Seq[Node])] = {
      val v = (elem.qname, elem.attributesByQName, elem.scope, elem.children)
      Some(v)
    }

    def from(elm: ScopedNodes.Elem): Elem = {
      val children = elm.children.collect {
        case childElm: ScopedNodes.Elem => childElm
        case childText: ScopedNodes.Text => childText
        case childComment: ScopedNodes.Comment => childComment
        case childProcessingInstruction: ScopedNodes.ProcessingInstruction => childProcessingInstruction
      }
      // Recursion, with Node.from and Elem.from being mutually dependent
      val simpleChildren = children.map { node => Node.from(node) }

      new Elem(elm.qname, elm.attributesByQName, elm.scope, simpleChildren.to(ArraySeq))
    }
  }

}
