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

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap
import scala.reflect.classTag

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.creationapi.ClarkNodeFactories
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractClarkElem
import eu.cdevreeze.yaidom2.updateapi.internal.AbstractUpdatableAttributeCarryingElem

/**
 * "Resolved" nodes.
 *
 * @author Chris de Vreeze
 */
object ResolvedNodes {

  // First the OO query API

  /**
   * Arbitrary resolved node
   */
  sealed trait Node extends ClarkNodes.Node

  /**
   * Potential document child
   */
  sealed trait CanBeDocumentChild extends Node with ClarkNodes.CanBeDocumentChild

  /**
   * "Resolved" element node, offering the `ClarkNodes.Elem` element query API.
   */
  final case class Elem(
    name: EName,
    attributes: SeqMap[EName, String],
    children: ArraySeq[Node]
  ) extends CanBeDocumentChild with AbstractClarkElem with AbstractUpdatableAttributeCarryingElem {

    type ThisElem = Elem

    type ThisNode = Node

    // Query API methods

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      ArraySeq.from(xs)(classTag[Elem])
    }

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e@Elem(_, _, _) if p(e) => e }
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e@Elem(_, _, _) if p(e) => e }
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

    // Update API methods

    def withChildren(newChildren: Seq[ThisNode]): ThisElem = {
      Elem(name, attributes, newChildren.to(ArraySeq))
    }

    def withAttributes(newAttributes: SeqMap[EName, String]): ThisElem = {
      Elem(name, newAttributes, children)
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
  }

  /**
   * "Resolved" text node
   */
  final case class Text(text: String) extends Node with ClarkNodes.Text

  // Next the functional query (and creation) API

  object Node extends ClarkNodeFactories.NodeFactory with ElemCreationApi {

    type NodeType = Node

    type ElemType = Elem

    type TargetNodeType = Node

    // NodeFactory method

    def from(node: ClarkNodes.Node): Node = node match {
      case e: ClarkNodes.Elem => Elem.from(e)
      case t: ClarkNodes.Text => Text(t.text)
      case n => sys.error(s"Not an element or text node: $n")
    }

    // ElemCreationApi methods

    def elem(name: EName, children: Seq[NodeType]): ElemType = {
      Elem(name, SeqMap.empty, children.to(ArraySeq))
    }

    def elem(name: EName, attributes: SeqMap[EName, String], children: Seq[NodeType]): ElemType = {
      Elem(name, attributes, children.to(ArraySeq))
    }

    def textElem(name: EName, txt: String): ElemType = {
      Elem(name, SeqMap.empty, ArraySeq(Text(txt)))
    }

    def textElem(name: EName, attributes: SeqMap[EName, String], txt: String): ElemType = {
      Elem(name, attributes, ArraySeq(Text(txt)))
    }

    def emptyElem(name: EName): ElemType = {
      Elem(name, SeqMap.empty, ArraySeq.empty)
    }

    def emptyElem(name: EName, attributes: SeqMap[EName, String]): ElemType = {
      Elem(name, attributes, ArraySeq.empty)
    }
  }

  object Elem extends ClarkNodeFactories.ElemFactory {

    type TargetElemType = Elem

    def from(elm: ClarkNodes.Elem): Elem = {
      val children = elm.children.collect {
        case childElm: ClarkNodes.Elem => childElm
        case childText: ClarkNodes.Text => childText
      }
      // Recursion, with Node.from and Elem.from being mutually dependent
      val resolvedChildren = children.map { node => Node.from(node) }

      Elem(elm.name, elm.attributes, resolvedChildren.to(ArraySeq))
    }
  }

}
