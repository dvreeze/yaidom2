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

import scala.collection.immutable
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap
import scala.collection.mutable
import scala.reflect.classTag

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.creationapi.ClarkNodeConverters
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.oo.internal.AbstractClarkElem
import eu.cdevreeze.yaidom2.queryapi.oofun.ClarkElemFunctionWrapper

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
  ) extends CanBeDocumentChild with AbstractClarkElem {

    type ThisElem = Elem

    type ThisNode = Node

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: mutable.Seq[Elem]): Seq[Elem] = {
      ArraySeq.from(xs)(classTag[Elem])
    }

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e@Elem(_, _, _) if p(e) => e }
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e@Elem(_, _, _) if p(e) => e }
    }
  }

  /**
   * "Resolved" text node
   */
  final case class Text(text: String) extends Node with ClarkNodes.Text

  // Next the functional query (and creation) API

  object Node extends ClarkNodeConverters.NodeConverter with ElemCreationApi {

    type NodeType = Node

    type ElemType = Elem

    type TargetNodeType = Node

    def from(node: ClarkNodes.Node): Node = node match {
      case e: ClarkNodes.Elem => Elem.from(e)
      case t: ClarkNodes.Text => Text(t.text)
      case n => sys.error(s"Not an element or text node: $n")
    }

    def elem(name: EName, children: Seq[NodeType]): ElemType = {
      Elem(name, SeqMap.empty, children.to(ArraySeq))
    }

    def elem(name: EName, attributes: immutable.Iterable[(EName, String)], children: Seq[NodeType]): ElemType = {
      Elem(name, attributes.to(SeqMap), children.to(ArraySeq))
    }

    def textElem(name: EName, txt: String): ElemType = {
      Elem(name, SeqMap.empty, ArraySeq(Text(txt)))
    }

    def textElem(name: EName, attributes: immutable.Iterable[(EName, String)], txt: String): ElemType = {
      Elem(name, attributes.to(SeqMap), ArraySeq(Text(txt)))
    }

    def emptyElem(name: EName): ElemType = {
      Elem(name, SeqMap.empty, ArraySeq.empty)
    }

    def emptyElem(name: EName, attributes: immutable.Iterable[(EName, String)]): ElemType = {
      Elem(name, attributes.to(SeqMap), ArraySeq.empty)
    }
  }

  object Elem extends ClarkElemFunctionWrapper with ClarkNodeConverters.ElemConverter {

    type ElemType = Elem

    type NodeType = Node

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
