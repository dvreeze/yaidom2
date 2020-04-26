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

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.creationapi.ScopedNodeFactories
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractScopedElem
import eu.cdevreeze.yaidom2.updateapi.internal.AbstractUpdatableElem

import scala.collection.immutable.ListMap

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
   *
   * Implementation note: this class used a ListMap for the attributes instead of VectorMap (via the SeqMap API), due to Scala issue
   * https://github.com/scala/scala/pull/8854.
   */
  final class Elem(
      val qname: QName,
      val attributesByQName: ListMap[QName, String],
      val scope: Scope,
      val children: Vector[Node] // For querying, ArraySeq would be optimal, but not for (functional) updates
  ) extends CanBeDocumentChild
      with AbstractScopedElem
      with AbstractUpdatableElem {

    // TODO Requirements on constructor parameters

    type ThisElem = Elem

    type ThisNode = Node

    val name: EName = {
      scope
        .resolveQNameOption(qname)
        .getOrElse(sys.error(s"Element name '$qname' should resolve to an EName in scope [$scope]"))
    }

    val attributes: ListMap[EName, String] = {
      collectAttributes((_, _) => true)
    }

    // Query API methods

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      Vector.from(xs)
    }

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e @ Elem(_, _, _, _) if p(e) => e }
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e @ Elem(_, _, _, _) if p(e) => e }
    }

    def findDescendantElemOrSelf(navigationPath: Seq[Int]): Option[ThisElem] = {
      if (navigationPath.isEmpty) {
        Some(self)
      } else {
        val childElemIdx: Int = navigationPath.head
        val childElems: Seq[Elem] = findAllChildElems

        if (childElemIdx >= 0 && childElemIdx < childElems.size) {
          // Recursive call
          Option(childElems(childElemIdx)).flatMap(_.findDescendantElemOrSelf(navigationPath.drop(1)))
        } else {
          None
        }
      }
    }

    override def localName: String = {
      qname.localPart
    }

    override def attrOption(attributeName: EName): Option[String] = {
      // TODO Still too expensive if the Scope has a default namespace, which is stripped to get the attribute scope
      val attributesWithSameLocalName: ListMap[EName, String] =
        collectAttributes((attName, _) => attName.localPart == attributeName.localPart)

      attributesWithSameLocalName.get(attributeName)
    }

    // Overriding methods that have type member ThisElem in the method signature, to "correct" the method signature now that ThisElem is known

    override def findAllChildElems: Seq[ThisElem] = super.findAllChildElems

    override def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = super.filterDescendantElems(p)

    override def findAllDescendantElems: Seq[ThisElem] = super.findAllDescendantElems

    override def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = super.findDescendantElem(p)

    override def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.filterDescendantElemsOrSelf(p)

    override def findAllDescendantElemsOrSelf: Seq[ThisElem] = super.findAllDescendantElemsOrSelf

    override def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = super.findDescendantElemOrSelf(p)

    override def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = super.findTopmostElems(p)

    override def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = super.findTopmostElemsOrSelf(p)

    override def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = super.getDescendantElemOrSelf(navigationPath)

    override def select(step: ElemStep[ThisElem]): Seq[ThisElem] = super.select(step)

    // Update API methods

    def withChildren(newChildren: Seq[ThisNode]): ThisElem = {
      new Elem(qname, attributesByQName, scope, newChildren.to(Vector))
    }

    protected def findAllChildElemsWithSteps: Seq[(ThisElem, Int)] = {
      findAllChildElems.zipWithIndex
    }

    override def updateChildElem(navigationStep: Int)(f: ThisElem => ThisElem): ThisElem = super.updateChildElem(navigationStep)(f)

    override def updateChildElem(navigationStep: Int, newElem: ThisElem): ThisElem = super.updateChildElem(navigationStep, newElem)

    override def updateChildElemWithNodeSeq(navigationStep: Int)(f: ThisElem => Seq[ThisNode]): ThisElem = {
      super.updateChildElemWithNodeSeq(navigationStep)(f)
    }

    override def updateChildElemWithNodeSeq(navigationStep: Int, newNodes: Seq[ThisNode]): ThisElem = {
      super.updateChildElemWithNodeSeq(navigationStep)(_ => newNodes)
    }

    override def updateDescendantElemOrSelf(navigationPath: Seq[Int])(f: ThisElem => ThisElem): ThisElem = {
      super.updateDescendantElemOrSelf(navigationPath)(f)
    }

    override def updateDescendantElemOrSelf(navigationPath: Seq[Int], newElem: ThisElem): ThisElem = {
      super.updateDescendantElemOrSelf(navigationPath, newElem)
    }

    override def updateDescendantElemWithNodeSeq(navigationPath: Seq[Int])(f: ThisElem => Seq[ThisNode]): ThisElem = {
      super.updateDescendantElemWithNodeSeq(navigationPath)(f)
    }

    override def updateDescendantElemWithNodeSeq(navigationPath: Seq[Int], newNodes: Seq[ThisNode]): ThisElem = {
      super.updateDescendantElemWithNodeSeq(navigationPath, newNodes)
    }

    override def updateChildElems(navigationSteps: Set[Int])(f: (ThisElem, Int) => ThisElem): ThisElem = {
      super.updateChildElems(navigationSteps)(f)
    }

    override def updateChildElemsWithNodeSeq(navigationSteps: Set[Int])(f: (ThisElem, Int) => Seq[ThisNode]): ThisElem = {
      super.updateChildElemsWithNodeSeq(navigationSteps)(f)
    }

    override def updateDescendantElemsOrSelf(navigationPaths: Set[Seq[Int]])(f: (ThisElem, Seq[Int]) => ThisElem): ThisElem = {
      super.updateDescendantElemsOrSelf(navigationPaths)(f)
    }

    override def updateDescendantElemsWithNodeSeq(navigationPaths: Set[Seq[Int]])(f: (ThisElem, Seq[Int]) => Seq[ThisNode]): ThisElem = {
      super.updateDescendantElemsWithNodeSeq(navigationPaths)(f)
    }

    // Transformation API methods

    def transformChildElems(f: ThisElem => ThisElem): ThisElem = {
      val resultChildNodes: Vector[ThisNode] =
        children.map {
          case e: Elem => f(e)
          case n       => n
        }

      withChildren(resultChildNodes)
    }

    def transformChildElemsToNodeSeq(f: ThisElem => Seq[ThisNode]): ThisElem = {
      val resultChildNodes: Vector[ThisNode] =
        children.flatMap {
          case e: Elem => f(e)
          case n       => Vector(n)
        }

      withChildren(resultChildNodes)
    }

    override def transformDescendantElemsOrSelf(f: ThisElem => ThisElem): ThisElem = super.transformDescendantElemsOrSelf(f)

    override def transformDescendantElems(f: ThisElem => ThisElem): ThisElem = super.transformDescendantElems(f)

    override def transformDescendantElemsOrSelfToNodeSeq(f: ThisElem => Seq[ThisNode]): Seq[ThisNode] = {
      super.transformDescendantElemsOrSelfToNodeSeq(f)
    }

    override def transformDescendantElemsToNodeSeq(f: ThisElem => Seq[ThisNode]): ThisElem = {
      super.transformDescendantElemsToNodeSeq(f)
    }

    // Other public methods

    def attributeScope: Scope = scope.withoutDefaultNamespace

    def withAttributesByQName(newAttributesByQName: ListMap[QName, String]): ThisElem = {
      new Elem(qname, newAttributesByQName, scope, children)
    }

    // Private methods

    private def collectAttributes(p: (QName, String) => Boolean): ListMap[EName, String] = {
      val attrScope = attributeScope

      attributesByQName.collect {
        case (attQName, attValue) if p(attQName, attValue) =>
          val attEName = attrScope
            .resolveQNameOption(attQName)
            .getOrElse(sys.error(s"Attribute name '$attQName' should resolve to an EName in scope [$attrScope]"))

          attEName -> attValue
      }
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

  object Node extends ScopedNodeFactories.NodeFactory {

    type TargetNodeType = Node

    def from(node: ScopedNodes.Node): Node = node match {
      case e: ScopedNodes.Elem                   => Elem.from(e)
      case t: ScopedNodes.Text                   => Text(t.text, isCData = false)
      case c: ScopedNodes.Comment                => Comment(c.text)
      case pi: ScopedNodes.ProcessingInstruction => ProcessingInstruction(pi.target, pi.data)
      case n                                     => sys.error(s"Not an element, text, comment or processing instruction node: $n")
    }
  }

  object CanBeDocumentChild {

    def from(node: ScopedNodes.CanBeDocumentChild): CanBeDocumentChild = node match {
      case e: ScopedNodes.Elem                   => Elem.from(e)
      case c: ScopedNodes.Comment                => Comment(c.text)
      case pi: ScopedNodes.ProcessingInstruction => ProcessingInstruction(pi.target, pi.data)
      case n                                     => sys.error(s"Not an element, comment or processing instruction node: $n")
    }
  }

  object Elem extends ScopedNodeFactories.ElemFactory {

    type ElemType = Elem

    type NodeType = Node

    type TargetElemType = Elem

    def unapply(elem: Elem): Option[(QName, ListMap[QName, String], Scope, Seq[Node])] = {
      val v = (elem.qname, elem.attributesByQName, elem.scope, elem.children)
      Some(v)
    }

    def from(elm: ScopedNodes.Elem): Elem = {
      val children = elm.children.collect {
        case childElm: ScopedNodes.Elem                                    => childElm
        case childText: ScopedNodes.Text                                   => childText
        case childComment: ScopedNodes.Comment                             => childComment
        case childProcessingInstruction: ScopedNodes.ProcessingInstruction => childProcessingInstruction
      }
      // Recursion, with Node.from and Elem.from being mutually dependent
      val simpleChildren = children.map { node =>
        Node.from(node)
      }

      new Elem(elm.qname, elm.attributesByQName, elm.scope, simpleChildren.to(Vector))
    }
  }

  /**
   * Returns a non-CDATA text with the given text string.
   */
  def text(textString: String): Text = Text(textString, isCData = false)
}
