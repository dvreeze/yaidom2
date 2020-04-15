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
import eu.cdevreeze.yaidom2.creationapi.ClarkNodeFactories
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractClarkElem
import eu.cdevreeze.yaidom2.updateapi.internal.AbstractUpdatableElem

import scala.collection.immutable.ListMap
import scala.collection.mutable

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
   *
   * Implementation note: this class used a ListMap for the attributes instead of VectorMap (via the SeqMap API), due to Scala issue
   * https://github.com/scala/scala/pull/8854.
   */
  final case class Elem(
      name: EName,
      attributes: ListMap[EName, String],
      children: Vector[Node] // For querying, ArraySeq would be optimal, but not for (functional) updates
  ) extends CanBeDocumentChild
      with AbstractClarkElem
      with AbstractUpdatableElem {

    type ThisElem = Elem

    type ThisNode = Node

    // Query API methods

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      Vector.from(xs)
    }

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e @ Elem(_, _, _) if p(e) => e }
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e @ Elem(_, _, _) if p(e) => e }
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
      Elem(name, attributes, newChildren.to(Vector))
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

    // Other methods

    /**
     * Returns a copy where inter-element whitespace has been removed, throughout the node tree.
     *
     * That is, for each descendant-or-self element determines if it has at least one child element and no non-whitespace
     * text child nodes, and if so, removes all (whitespace) text children.
     *
     * This method is useful if it is known that whitespace around element nodes is used for formatting purposes, and (in
     * the absence of an XML Schema or DTD) can therefore be treated as "ignorable whitespace". In the case of "mixed content"
     * (if text around element nodes is not all whitespace), this method will not remove any text children of the parent element.
     *
     * XML space attributes (xml:space) are not respected by this method. If such whitespace preservation functionality is needed,
     * it can be written as a transformation where for specific elements this method is not called.
     */
    def removeAllInterElementWhitespace: Elem = {
      def isWhitespaceText(n: Node): Boolean = n match {
        case t: Text if t.text.trim.isEmpty => true
        case _                              => false
      }

      def isNonTextNode(n: Node): Boolean = n match {
        case _: Text => false
        case _       => true
      }

      val doStripWhitespace = findChildElem(_ => true).nonEmpty && children.forall(n => isWhitespaceText(n) || isNonTextNode(n))

      // Recursive, but not tail-recursive

      val newChildren = {
        val remainder = if (doStripWhitespace) children.filter(n => isNonTextNode(n)) else children

        remainder.map {
          case e: Elem =>
            // Recursive call
            e.removeAllInterElementWhitespace
          case n =>
            n
        }
      }

      withChildren(newChildren)
    }

    /**
     * Returns a copy where adjacent text nodes have been combined into one text node, throughout the node tree.
     * After combining the adjacent text nodes, all text nodes are transformed by calling the passed function.
     */
    def coalesceAllAdjacentTextAndPostprocess(f: Text => Text): Elem = {
      // Recursive, but not tail-recursive

      @scala.annotation.tailrec
      def accumulate(childNodes: Seq[Node], newChildrenBuffer: mutable.ArrayBuffer[Node]): Unit = {
        if (childNodes.nonEmpty) {
          val head = childNodes.head

          head match {
            case _: Text =>
              val (textNodes, remainder) = childNodes.span {
                case _: Text => true
                case _       => false
              }

              val combinedText: String = textNodes.collect { case t: Text => t.text }.mkString("")

              newChildrenBuffer += f(Text(combinedText))

              // Recursive call
              accumulate(remainder, newChildrenBuffer)
            case n: Node =>
              newChildrenBuffer += n

              // Recursive call
              accumulate(childNodes.tail, newChildrenBuffer)
          }
        }
      }

      transformDescendantElemsOrSelf { elm =>
        val newChildrenBuffer = mutable.ArrayBuffer[Node]()

        accumulate(elm.children, newChildrenBuffer)

        elm.withChildren(newChildrenBuffer.toIndexedSeq)
      }
    }

    /** Returns a copy where adjacent text nodes have been combined into one text node, throughout the node tree */
    def coalesceAllAdjacentText: Elem = {
      coalesceAllAdjacentTextAndPostprocess(t => t)
    }

    /**
     * Returns a copy where adjacent text nodes have been combined into one text node, and where all
     * text is normalized, throughout the node tree.
     */
    def coalesceAndNormalizeAllText: Elem = {
      coalesceAllAdjacentTextAndPostprocess(t => Text(normalizeString(t.text)))
    }

    /**
     * Normalizes the string, removing surrounding whitespace and normalizing internal whitespace to a single space.
     * Whitespace includes #x20 (space), #x9 (tab), #xD (carriage return), #xA (line feed). If there is only whitespace,
     * the empty string is returned. Inspired by the JDOM library.
     */
    private def normalizeString(s: String): String = {
      require(s ne null) // scalastyle:off null

      val separators = Array(' ', '\t', '\r', '\n')
      val words: Seq[String] = s.split(separators).toSeq.filterNot(_.isEmpty)

      words.mkString(" ") // Returns empty string if words.isEmpty
    }
  }

  /**
   * "Resolved" text node
   */
  final case class Text(text: String) extends Node with ClarkNodes.Text

  // Next the functional query (and creation) API

  object Node extends ClarkNodeFactories.NodeFactory {

    type TargetNodeType = Node

    // NodeFactory method

    def from(node: ClarkNodes.Node): Node = node match {
      case e: ClarkNodes.Elem => Elem.from(e)
      case t: ClarkNodes.Text => Text(t.text)
      case n                  => sys.error(s"Not an element or text node: $n")
    }
  }

  object Elem extends ClarkNodeFactories.ElemFactory {

    type TargetElemType = Elem

    def from(elm: ClarkNodes.Elem): Elem = {
      val children = elm.children.collect {
        case childElm: ClarkNodes.Elem  => childElm
        case childText: ClarkNodes.Text => childText
      }
      // Recursion, with Node.from and Elem.from being mutually dependent
      val resolvedChildren = children.map { node =>
        Node.from(node)
      }

      Elem(elm.name, elm.attributes, resolvedChildren.to(Vector))
    }
  }

}
