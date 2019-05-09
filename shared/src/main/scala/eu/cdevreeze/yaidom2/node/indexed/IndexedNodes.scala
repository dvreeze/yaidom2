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

package eu.cdevreeze.yaidom2.node.indexed

import java.net.URI

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap
import scala.reflect.classTag

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.creationapi.BackingNodeConverters
import eu.cdevreeze.yaidom2.node.simple.SimpleNodes
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.queryapi.oo.internal.AbstractBackingElem
import eu.cdevreeze.yaidom2.queryapi.oofun.BackingElemFunctionWrapper

/**
 * Native "indexed" nodes.
 *
 * @author Chris de Vreeze
 */
object IndexedNodes {

  /**
   * Arbitrary indexed node
   */
  sealed trait Node extends BackingNodes.Node

  /**
   * Potential indexed document child, so an element, processing instruction or comment
   */
  sealed trait CanBeDocumentChild extends Node with BackingNodes.CanBeDocumentChild

  /**
   * Indexed element node, offering the `BackingNodes.Elem` element query API.
   *
   * The element navigation path is the element's navigation path relative to the underlying root element.
   * Each entry in the element navigation path is a (zero-based) child element index, not a child node index!
   */
  final class Elem private(
    val docUriOption: Option[URI],
    val underlyingRootElem: SimpleNodes.Elem,
    val elemNavigationPathFromRoot: ArraySeq[Int],
    val underlyingElem: SimpleNodes.Elem
  ) extends CanBeDocumentChild with AbstractBackingElem {

    type ThisElem = Elem

    type ThisNode = Node

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      ArraySeq.from(xs)(classTag[Elem])
    }

    override def equals(other: Any): Boolean = other match {
      case otherElem: Elem =>
        otherElem.docUriOption == docUriOption && otherElem.underlyingRootElem == underlyingRootElem &&
          otherElem.elemNavigationPathFromRoot == elemNavigationPathFromRoot && otherElem.underlyingElem == underlyingElem
      case _ =>
        false
    }

    override def hashCode: Int = {
      (docUriOption, underlyingRootElem, elemNavigationPathFromRoot, underlyingElem).hashCode()
    }

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      underlyingElem.findAllChildElems().zipWithIndex
        .map { case (e, idx) =>
          new Elem(docUriOption, underlyingRootElem, elemNavigationPathFromRoot.appended(idx), e)
        }.filter(p)
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      filterChildElems(p).headOption
    }

    def name: EName = {
      underlyingElem.name
    }

    def attributes: SeqMap[EName, String] = {
      underlyingElem.attributes
    }

    def scope: Scope = {
      underlyingElem.scope
    }

    def qname: QName = {
      underlyingElem.qname
    }

    def attributesByQName: SeqMap[QName, String] = {
      underlyingElem.attributesByQName
    }

    def findParentElem(p: ThisElem => Boolean): Option[ThisElem] = {
      if (elemNavigationPathFromRoot.isEmpty) {
        None
      } else {
        val parentPath = elemNavigationPathFromRoot.init
        val underlyingParentElem = underlyingRootElem.atNavigationPath(parentPath)
        val parentElem = new Elem(docUriOption, underlyingRootElem, parentPath, underlyingParentElem)
        Some(parentElem).filter(p)
      }
    }

    def rootElem: ThisElem = {
      new Elem(docUriOption, underlyingRootElem, ArraySeq.empty, underlyingRootElem)
    }

    def children: Seq[ThisNode] = {
      var childElemIdx = 0

      underlyingElem.children.map { node =>
        node match {
          case che: SimpleNodes.Elem =>
            val idx = childElemIdx
            childElemIdx += 1
            new Elem(docUriOption, underlyingRootElem, elemNavigationPathFromRoot.appended(idx), che)
          case SimpleNodes.Text(t, isCData) =>
            Text(t, isCData)
          case SimpleNodes.Comment(t) =>
            Comment(t)
          case SimpleNodes.ProcessingInstruction(target, data) =>
            ProcessingInstruction(target, data)
        }
      }
    }
  }

  /**
   * "Indexed" text node
   */
  final case class Text(text: String, isCData: Boolean) extends Node with BackingNodes.Text

  /**
   * "Indexed" comment node
   */
  final case class Comment(text: String) extends CanBeDocumentChild with BackingNodes.Comment

  /**
   * "Indexed" processing instruction
   */
  final case class ProcessingInstruction(target: String, data: String) extends CanBeDocumentChild with BackingNodes.ProcessingInstruction

  object Node extends BackingNodeConverters.NodeConverter {

    type TargetNodeType = Node

    def from(node: BackingNodes.Node): Node = node match {
      case e: BackingNodes.Elem => Elem.from(e)
      case t: BackingNodes.Text => Text(t.text, false)
      case c: BackingNodes.Comment => Comment(c.text)
      case pi: BackingNodes.ProcessingInstruction => ProcessingInstruction(pi.target, pi.data)
      case n => sys.error(s"Not an element, text, comment or processing instruction node: $n")
    }
  }

  object Elem extends BackingElemFunctionWrapper with BackingNodeConverters.ElemConverter {

    type ElemType = Elem

    type NodeType = Node

    type TargetElemType = Elem

    def from(elm: BackingNodes.Elem): Elem = {
      val docUriOption: Option[URI] = elm.docUriOption

      val simpleRootElem = SimpleNodes.Elem.from(elm.rootElem)

      val elemNavigationPathFromRoot: ArraySeq[Int] = computeElemNavigationPathFromRoot(elm)

      of(docUriOption, simpleRootElem, elemNavigationPathFromRoot)
    }

    /**
     * Creation method for indexed elements.
     */
    def of(docUriOption: Option[URI], underlyingRootElem: SimpleNodes.Elem, elemNavigationPathFromRoot: Seq[Int]): Elem = {
      new Elem(
        docUriOption,
        underlyingRootElem,
        elemNavigationPathFromRoot.to(ArraySeq),
        underlyingRootElem.atNavigationPath(elemNavigationPathFromRoot))
    }

    def ofRoot(docUriOption: Option[URI], underlyingRootElem: SimpleNodes.Elem): Elem = {
      of(docUriOption, underlyingRootElem, Seq.empty)
    }

    private def computeElemNavigationPathFromRoot(elm: BackingNodes.Elem): ArraySeq[Int] = {
      def computeReversePath(e: BackingNodes.Elem): List[Int] = {
        val parentElemOption = e.findParentElem()

        if (parentElemOption.isEmpty) {
          Nil
        } else {
          // Recursive call
          e.findAllPrecedingSiblingElems().size :: computeReversePath(parentElemOption.get)
        }
      }

      computeReversePath(elm).reverse.to(ArraySeq)
    }
  }

}
