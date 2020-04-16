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

import java.net.URI

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.PrefixedScope
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.creationapi.ScopedDocumentFactory
import eu.cdevreeze.yaidom2.creationapi.ScopedNodeFactories
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.Nodes
import eu.cdevreeze.yaidom2.queryapi.ScopedDocumentApi
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.elemstep.ScopedElemStepFactory
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractScopedElem
import eu.cdevreeze.yaidom2.updateapi.internal.AbstractUpdatableElem

import scala.collection.immutable.ListMap

/**
 * "Creation DSL" nodes and documents.
 *
 * @author Chris de Vreeze
 */
object NodeBuilders {

  /**
   * Document holding a NodeBuilders.Elem.
   */
  final case class Document(docUriOption: Option[URI], children: Seq[CanBeDocumentChild]) extends ScopedDocumentApi {
    require(
      children.collect { case e: Elem => e }.size == 1,
      s"A document must have precisely 1 document element but found ${children.collect { case e: Elem => e }.size} ones")

    type NodeType = Node

    type CanBeDocumentChildType = CanBeDocumentChild

    type ElemType = Elem

    def documentElement: ElemType = children.collectFirst { case e: Elem => e }.get
  }

  object Document extends ScopedDocumentFactory {

    type TargetDocumentType = Document

    def apply(docUriOption: Option[URI], documentElement: Elem): Document = {
      apply(docUriOption, Seq(documentElement))
    }

    def from(document: ScopedDocumentApi): Document = {
      val docChildren = document.children.collect { case ch: ScopedNodes.CanBeDocumentChild => ch }

      val targetDocChildren =
        docChildren
          .filter(n => Set[Nodes.NodeKind](Nodes.ElementKind).contains(n.nodeKind))
          .map(n => Elem.from(n.asInstanceOf[ScopedNodes.Elem])) // TODO Replace this expensive conversion

      Document(document.docUriOption, targetDocChildren)
    }
  }

  // First the OO query API

  /**
   * Arbitrary creation DSL node
   */
  sealed trait Node extends ScopedNodes.Node

  /**
   * Potential document child
   */
  sealed trait CanBeDocumentChild extends Node with ScopedNodes.CanBeDocumentChild

  /**
   * "Creation DSL" element node, offering the `ScopedNodes.Elem` element query API.
   *
   * Implementation note: this class used a ListMap for the attributes instead of VectorMap (via the SeqMap API), due to Scala issue
   * https://github.com/scala/scala/pull/8854.
   */
  final class Elem private[nodebuilder] (
      val name: EName,
      val attributes: ListMap[EName, String],
      val children: Vector[Node], // For querying, ArraySeq would be optimal, but not for (functional) updates
      val prefixedScope: PrefixedScope
  ) extends CanBeDocumentChild
      with AbstractScopedElem
      with AbstractUpdatableElem {

    // TODO These are expensive checks. Improve performance if possible.
    assert(
      Elem.hasElementAndAttributeQNames(name, attributes, prefixedScope),
      s"No QName(s) for element name '$name' and/or attribute names (scope: $prefixedScope)"
    )
    assert(this.hasNoPrefixedNamespaceUndeclarations, s"Prefixed namespace undeclarations found but not allowed, for element '$name''")

    type ThisElem = Elem

    type ThisNode = Node

    // Query API methods

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      Vector.from(xs)
    }

    def scope: Scope = prefixedScope.scope

    /**
     * Returns the QName. If prefixed, the prefix is the "minimal" one in string comparison order for the namespace of the EName.
     */
    def qname: QName = {
      val prefixOption: Option[String] =
        if (name.namespaceUriOption.isEmpty) None else prefixedScope.findPrefixForNamespace(name.namespaceUriOption.get)

      QName(prefixOption, name.localPart)
    }

    /**
     * Returns the attributes by QName. For any attribute name, if prefixed, the prefix is the "minimal" one in string comparison order
     * for the namespace of the attribute EName.
     */
    def attributesByQName: ListMap[QName, String] = {
      attributes.map {
        case (attrName, attrValue) =>
          val prefixOption: Option[String] =
            if (attrName.namespaceUriOption.isEmpty) None else prefixedScope.findPrefixForNamespace(attrName.namespaceUriOption.get)

          QName(prefixOption, attrName.localPart) -> attrValue
      }
    }

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e: Elem if p(e) => e }
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e: Elem if p(e) => e }
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

    /**
     * Replaces the children by the given child nodes. Throws an exception if at least one prefixed namespace undeclaration
     * would be introduced. Therefore it is better to use the "element creation DSL" instead.
     */
    def withChildren(newChildren: Seq[ThisNode]): ThisElem = {
      val introducesNoPrefixedNamespaceUndeclarations: Boolean =
        newChildren.collect { case e: Elem => e }.forall { che =>
          prefixedScope.scope.relativize(che.prefixedScope.scope).retainingUndeclarations.isEmpty
        }
      require(
        introducesNoPrefixedNamespaceUndeclarations,
        s"Not all child elements introduce no (prefixed) namespace undeclarations (current element's scope: $scope)"
      )

      new Elem(name, attributes, newChildren.to(Vector), prefixedScope)
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
  }

  /**
   * "Creation DSL" text node
   */
  final case class Text(text: String) extends Node with ScopedNodes.Text

  /**
   * "Creation DSL" comment node
   */
  final case class Comment(text: String) extends CanBeDocumentChild with ScopedNodes.Comment

  /**
   * "Creation DSL" processing instruction
   */
  final case class ProcessingInstruction(target: String, data: String) extends CanBeDocumentChild with ScopedNodes.ProcessingInstruction

  // Companion objects

  object Node extends ScopedNodeFactories.NodeFactory {

    type TargetNodeType = Node

    def optionallyFrom(node: ScopedNodes.Node): Option[Node] = node match {
      case e: ScopedNodes.Elem                   => Elem.optionallyFrom(e)
      case t: ScopedNodes.Text                   => Some(Text(t.text))
      case c: ScopedNodes.Comment                => Some(Comment(c.text))
      case pi: ScopedNodes.ProcessingInstruction => Some(ProcessingInstruction(pi.target, pi.data))
      case _                                     => None
    }

    def from(node: ScopedNodes.Node): Node = node match {
      case e: ScopedNodes.Elem                   => Elem.from(e)
      case t: ScopedNodes.Text                   => Text(t.text)
      case c: ScopedNodes.Comment                => Comment(c.text)
      case pi: ScopedNodes.ProcessingInstruction => ProcessingInstruction(pi.target, pi.data)
      case n                                     => sys.error(s"Not an element, text, comment or PI node: $n")
    }
  }

  object Elem extends ScopedNodeFactories.ElemFactory {

    type ElemType = Elem

    type NodeType = Node

    type TargetElemType = Elem

    def unapply(elem: Elem): Option[(EName, ListMap[EName, String], Seq[Node], PrefixedScope)] = {
      val v = (elem.name, elem.attributes, elem.children, elem.prefixedScope)
      Some(v)
    }

    /**
     * Returns an optional copy of the passed element as "creation DSL" element.
     * If the passed element does not meet the requirements of "creation DSL" elements, None is returned.
     * Such requirements include the absence of any default namespace, the absence of namespace undeclarations, etc.
     */
    def optionallyFrom(elm: ScopedNodes.Elem): Option[Elem] = {
      val allElemsOrSelf = elm.findAllDescendantElemsOrSelf

      def isItselfValid(e: ScopedNodes.Elem): Boolean = {
        PrefixedScope.optionallyFrom(e.scope).nonEmpty &&
        hasElementAndAttributeQNames(e.name, e.attributes, PrefixedScope.from(e.scope)) && // this should be true anyway
        e.hasNoPrefixedNamespaceUndeclarations
      }

      if (allElemsOrSelf.forall(isItselfValid)) {
        Some(from(elm))
      } else {
        None
      }
    }

    def from(elm: ScopedNodes.Elem): Elem = {
      require(elm.scope.defaultNamespaceOption.isEmpty, s"Not a scope without default namespace: ${elm.scope}")
      val prefixedScope = PrefixedScope.from(elm.scope)

      require(
        hasElementAndAttributeQNames(elm.name, elm.attributes, prefixedScope),
        s"Element ${elm.name} with scope ${elm.scope} is corrupt with respect to naming (the correspondence between QNames and ENames)"
      )

      require(
        elm.hasNoPrefixedNamespaceUndeclarations,
        s"Element ${elm.name} with scope ${elm.scope} has namespace undeclarations, which is not allowed")

      val children = elm.children.collect {
        case childElm: ScopedNodes.Elem                 => childElm
        case childText: ScopedNodes.Text                => childText
        case childComment: ScopedNodes.Comment          => childComment
        case childPi: ScopedNodes.ProcessingInstruction => childPi
      }

      // Recursion, with Node.from and Elem.from being mutually dependent
      val creationDslChildren = children.map { node =>
        Node.from(node)
      }

      new Elem(elm.name, elm.attributes, creationDslChildren.to(Vector), prefixedScope)
    }

    // Constraints on element builders (besides the use of PrefixedScopes only)

    // Property ScopedNodes.Elem.hasNoPrefixedNamespaceUndeclarations is required too

    /**
     * Returns true if both element name (as EName) and attribute names (as ENames) determine a corresponding
     * QName, given the passed simple scope. This is a requirement that has to be fulfilled in order to create an
     * element builder containing this data, and it must hold for all descendant elements as well.
     */
    def hasElementAndAttributeQNames(elementName: EName, attributes: ListMap[EName, String], prefixedScope: PrefixedScope): Boolean = {
      hasElementQName(elementName, prefixedScope) &&
      hasAttributeQNames(attributes, prefixedScope)
    }

    /**
     * Returns true if the element name (as EName) determines a corresponding QName, given the passed simple scope.
     */
    def hasElementQName(elementName: EName, prefixedScope: PrefixedScope): Boolean = {
      hasQName(elementName, prefixedScope)
    }

    /**
     * Returns true if the attribute names (as ENames) determine a corresponding QName, given the passed simple scope.
     */
    def hasAttributeQNames(attributes: ListMap[EName, String], prefixedScope: PrefixedScope): Boolean = {
      attributes.forall {
        case (attrName, _) =>
          hasQName(attrName, prefixedScope)
      }
    }

    /**
     * Returns true if the given EName determines a corresponding QName, given the passed simple scope.
     */
    def hasQName(ename: EName, prefixedScope: PrefixedScope): Boolean = {
      prefixedScope.findQName(ename).nonEmpty
    }
  }

  // The element step factory class

  /**
   * ElemStep factory API for creation DSL elements.
   */
  object ElemSteps extends ScopedElemStepFactory {

    type ElemType = Elem
  }
}
