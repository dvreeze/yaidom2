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

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.core.SimpleScope
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi
import eu.cdevreeze.yaidom2.creationapi.ScopedDocumentFactory
import eu.cdevreeze.yaidom2.creationapi.ScopedNodeFactories
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.Nodes
import eu.cdevreeze.yaidom2.queryapi.ScopedDocumentApi
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.elemstep.ScopedElemStepFactory
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractScopedElem
import eu.cdevreeze.yaidom2.updateapi.internal.AbstractUpdatableAttributeCarryingElem

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
        docChildren.filter(n => Set[Nodes.NodeKind](Nodes.ElementKind).contains(n.nodeKind))
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
   */
  final class Elem private[nodebuilder](
    val name: EName,
    val attributes: SeqMap[EName, String],
    val children: Vector[Node], // For querying, ArraySeq would be optimal, but not for (functional) updates
    val simpleScope: SimpleScope
  ) extends CanBeDocumentChild with AbstractScopedElem with AbstractUpdatableAttributeCarryingElem {

    assert(Elem.hasValidQNamesCorrespondingToENames(name, attributes, simpleScope))
    assert(this.hasNoPrefixedNamespaceUndeclarations)

    type ThisElem = Elem

    type ThisNode = Node

    // Query API methods

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      Vector.from(xs)
    }

    def scope: Scope = simpleScope.scope

    def qname: QName = {
      val prefixOption: Option[String] =
        if (name.namespaceUriOption.isEmpty) None else simpleScope.findPrefixForNamespace(name.namespaceUriOption.get)

      QName(prefixOption, name.localPart)
    }

    def attributesByQName: SeqMap[QName, String] = {
      attributes.map { case (attrName, attrValue) =>
        val prefixOption: Option[String] =
          if (attrName.namespaceUriOption.isEmpty) None else simpleScope.findPrefixForNamespace(attrName.namespaceUriOption.get)

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
        val childElemIdx: Int = navigationPath(0)
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
      require(
        newChildren.collect { case e: Elem => e }.forall(e => scope.subScopeOf(e.scope)),
        s"Not all child elements have a strict super-scope of $scope")

      new Elem(name, attributes, newChildren.to(Vector), simpleScope)
    }

    def withAttributes(newAttributes: SeqMap[EName, String]): ThisElem = {
      require(
        newAttributes.keySet.forall(attrName => simpleScope.findQName(attrName).nonEmpty),
        s"Not all attribute names can be converted to QNames given scope $scope")

      new Elem(name, newAttributes, children, simpleScope)
    }

    protected def findAllChildElemsWithSteps: Seq[(ThisElem, Int)] = {
      findAllChildElems.zipWithIndex
    }

    override def plusChild(child: ThisNode): ThisElem = super.plusChild(child)

    override def plusChildOption(childOption: Option[ThisNode]): ThisElem = super.plusChildOption(childOption)

    override def plusChild(index: Int, child: ThisNode): ThisElem = super.plusChild(index, child)

    override def plusChildOption(index: Int, childOption: Option[ThisNode]): ThisElem = super.plusChildOption(index, childOption)

    override def plusChildren(childSeq: Seq[ThisNode]): ThisElem = super.plusChildren(childSeq)

    override def minusChild(index: Int): ThisElem = super.minusChild(index)

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

    override def updateChildElems(f: PartialFunction[(ThisElem, Int), ThisElem]): ThisElem = super.updateChildElems(f)

    override def updateChildElemsWithNodeSeq(f: PartialFunction[(ThisElem, Int), Seq[ThisNode]]): ThisElem = super.updateChildElemsWithNodeSeq(f)

    override def updateTopmostElemsOrSelf(f: PartialFunction[(ThisElem, Seq[Int]), ThisElem]): ThisElem = {
      super.updateTopmostElemsOrSelf(f)
    }

    override def updateTopmostElemsWithNodeSeq(f: PartialFunction[(ThisElem, Seq[Int]), Seq[ThisNode]]): ThisElem = {
      super.updateTopmostElemsWithNodeSeq(f)
    }

    override def plusAttribute(attrName: EName, attrValue: String): ThisElem = super.plusAttribute(attrName, attrValue)

    override def plusAttributeOption(attrName: EName, attrValueOption: Option[String]): ThisElem = {
      super.plusAttributeOption(attrName, attrValueOption)
    }

    override def plusAttributes(newAttributes: SeqMap[EName, String]): ThisElem = super.plusAttributes(newAttributes)

    override def minusAttribute(attrName: EName): ThisElem = super.minusAttribute(attrName)

    // Transformation API methods

    def transformChildElems(f: ThisElem => ThisElem): ThisElem = {
      val resultChildNodes: Vector[ThisNode] =
        children.map {
          case e: Elem => f(e)
          case n => n
        }

      withChildren(resultChildNodes)
    }

    def transformChildElemsToNodeSeq(f: ThisElem => Seq[ThisNode]): ThisElem = {
      val resultChildNodes: Vector[ThisNode] =
        children.flatMap {
          case e: Elem => f(e)
          case n => Vector(n)
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

    def deeplyEnhancingScopeWith(extraScope: SimpleScope): Elem = {
      transformDescendantElemsOrSelf { e =>
        new Elem(e.name, e.attributes, e.children, e.simpleScope.appendAggressivelyOrThrow(extraScope))
      }
    }
  }

  /**
   * "Creation DSL" text node
   */
  final case class Text(text: String) extends Node with ScopedNodes.Text

  // Companion objects

  object Node extends ScopedNodeFactories.NodeFactory {

    type TargetNodeType = Node

    def optionallyFrom(node: ScopedNodes.Node): Option[Node] = node match {
      case e: ScopedNodes.Elem => Elem.optionallyFrom(e)
      case t: ScopedNodes.Text => Some(Text(t.text))
      case _ => None
    }

    def from(node: ScopedNodes.Node): Node = node match {
      case e: ScopedNodes.Elem => Elem.from(e)
      case t: ScopedNodes.Text => Text(t.text)
      case n => sys.error(s"Not an element or text node: $n")
    }
  }

  object Elem extends ScopedNodeFactories.ElemFactory {

    type ElemType = Elem

    type NodeType = Node

    type TargetElemType = Elem

    def unapply(elem: Elem): Option[(EName, SeqMap[EName, String], Seq[Node], SimpleScope)] = {
      val v = (elem.name, elem.attributes, elem.children, elem.simpleScope)
      Some(v)
    }

    def optionallyFrom(elm: ScopedNodes.Elem): Option[Elem] = {
      val allElemsOrSelf = elm.findAllDescendantElemsOrSelf

      def isItselfValid(e: ScopedNodes.Elem): Boolean = {
        SimpleScope.optionallyFrom(e.scope).nonEmpty &&
          hasValidQNamesCorrespondingToENames(e.name, e.attributes, SimpleScope.from(e.scope)) &&
          e.hasNoPrefixedNamespaceUndeclarations
      }

      if (allElemsOrSelf.forall(isItselfValid)) {
        Some(from(elm))
      } else {
        None
      }
    }

    def from(elm: ScopedNodes.Elem): Elem = {
      require(elm.scope.isInvertible, s"Not an invertible scope: ${elm.scope}")
      require(elm.scope.defaultNamespaceOption.isEmpty, s"Not a scope without default namespace: ${elm.scope}")
      val simpleScope = SimpleScope.from(elm.scope)

      require(
        hasValidQNamesCorrespondingToENames(elm.name, elm.attributes, simpleScope),
        s"Element ${elm.name} with scope ${elm.scope} is corrupt with respect to naming (the correspondence between QNames and ENames)")

      require(elm.hasNoPrefixedNamespaceUndeclarations, s"Element ${elm.name} with scope ${elm.scope} has namespace undeclarations, which is not allowed")

      val children = elm.children.collect {
        case childElm: ScopedNodes.Elem =>
          childElm
        case childText: ScopedNodes.Text =>
          childText
      }

      // Recursion, with Node.from and Elem.from being mutually dependent
      val creationDslChildren = children.map { node => Node.from(node) }

      new Elem(elm.name, elm.attributes, creationDslChildren.to(Vector), simpleScope)
    }

    // Constraints on element builders (besides the use of SimpleScopes only)

    // Property ScopedNodes.Elem.hasNoPrefixedNamespaceUndeclarations is required too

    /**
     * Returns true if both element name (as EName) and attribute names (as ENames) uniquely determine a corresponding
     * QName, given the passed simple scope. This is a requirement that has to be fulfilled in order to create an
     * element builder containing this data, and it must hold for all descendant elements as well.
     */
    def hasValidQNamesCorrespondingToENames(elementName: EName, attributes: SeqMap[EName, String], simpleScope: SimpleScope): Boolean = {
      hasValidElementQNamesCorrespondingToEName(elementName, simpleScope) &&
        hasValidAttributeQNamesCorrespondingToENames(attributes, simpleScope)
    }

    /**
     * Returns true if the element name (as EName) uniquely determines a corresponding QName, given the passed simple scope.
     */
    def hasValidElementQNamesCorrespondingToEName(elementName: EName, simpleScope: SimpleScope): Boolean = {
      hasValidQNameCorrespondingToEName(elementName, simpleScope)
    }

    /**
     * Returns true if the attribute names (as ENames) uniquely determine a corresponding QName, given the passed simple scope.
     */
    def hasValidAttributeQNamesCorrespondingToENames(attributes: SeqMap[EName, String], simpleScope: SimpleScope): Boolean = {
      attributes.forall { case (attrName, _) =>
        hasValidQNameCorrespondingToEName(attrName, simpleScope)
      }
    }

    /**
     * Returns true if the given EName uniquely determines a corresponding QName, given the passed simple scope.
     */
    def hasValidQNameCorrespondingToEName(ename: EName, simpleScope: SimpleScope): Boolean = {
      simpleScope.findQName(ename).nonEmpty
    }
  }

  // The element step factory class

  /**
   * ElemStep factory API for creation DSL elements.
   */
  object ElemSteps extends ScopedElemStepFactory {

    type ElemType = Elem
  }

  // The element creation API implementation

  /**
   * Element creation API for "creation DSL" elements.
   *
   * @author Chris de Vreeze
   */
  final case class ElemCreator(simpleScope: SimpleScope) extends ElemCreationApi {

    type NodeType = Node

    type ElemType = Elem

    // TODO Checks on prefixed namespace undeclarations

    /**
     * Returns `ElemCreator(simpleScope.appendDefensively(otherSimpleScope))`.
     */
    def appendDefensively(otherSimpleScope: SimpleScope): ElemCreator = {
      ElemCreator(simpleScope.appendDefensively(otherSimpleScope))
    }

    /**
     * Returns `ElemCreator(simpleScope.appendAggressively(otherSimpleScope))`.
     */
    def appendAggressively(otherSimpleScope: SimpleScope): ElemCreator = {
      ElemCreator(simpleScope.appendAggressively(otherSimpleScope))
    }

    /**
     * Returns `ElemCreator(simpleScope.appendDefensivelyOrThrow(otherSimpleScope))`.
     */
    def appendDefensivelyOrThrow(otherSimpleScope: SimpleScope): ElemCreator = {
      ElemCreator(simpleScope.appendDefensivelyOrThrow(otherSimpleScope))
    }

    /**
     * Returns `ElemCreator(simpleScope.appendAggressivelyOrThrow(otherSimpleScope))`.
     */
    def appendAggressivelyOrThrow(otherSimpleScope: SimpleScope): ElemCreator = {
      ElemCreator(simpleScope.appendAggressivelyOrThrow(otherSimpleScope))
    }

    def elem(name: EName, children: Seq[NodeType]): ElemType = {
      require(
        Elem.hasValidElementQNamesCorrespondingToEName(name, simpleScope),
        s"Could not turn element name $name into a QName (scope $simpleScope)")

      new Elem(name, SeqMap.empty, children.to(Vector), simpleScope)
    }

    def elem(name: EName, attributes: SeqMap[EName, String], children: Seq[NodeType]): ElemType = {
      require(
        Elem.hasValidElementQNamesCorrespondingToEName(name, simpleScope),
        s"Could not turn element name $name into a QName (scope $simpleScope)")
      require(
        Elem.hasValidAttributeQNamesCorrespondingToENames(attributes, simpleScope),
        s"Could not turn all attribute names into QNames (element $name, scope $simpleScope)")

      new Elem(name, attributes, children.to(Vector), simpleScope)
    }

    def textElem(name: EName, txt: String): ElemType = {
      require(
        Elem.hasValidElementQNamesCorrespondingToEName(name, simpleScope),
        s"Could not turn element name $name into a QName (scope $simpleScope)")

      new Elem(name, SeqMap.empty, Vector(Text(txt)), simpleScope)
    }

    def textElem(name: EName, attributes: SeqMap[EName, String], txt: String): ElemType = {
      require(
        Elem.hasValidElementQNamesCorrespondingToEName(name, simpleScope),
        s"Could not turn element name $name into a QName (scope $simpleScope)")
      require(
        Elem.hasValidAttributeQNamesCorrespondingToENames(attributes, simpleScope),
        s"Could not turn all attribute names into QNames (element $name, scope $simpleScope)")

      new Elem(name, attributes, Vector(Text(txt)), simpleScope)
    }

    def emptyElem(name: EName): ElemType = {
      require(
        Elem.hasValidElementQNamesCorrespondingToEName(name, simpleScope),
        s"Could not turn element name $name into a QName (scope $simpleScope)")

      new Elem(name, SeqMap.empty, Vector.empty, simpleScope)
    }

    def emptyElem(name: EName, attributes: SeqMap[EName, String]): ElemType = {
      require(
        Elem.hasValidElementQNamesCorrespondingToEName(name, simpleScope),
        s"Could not turn element name $name into a QName (scope $simpleScope)")
      require(
        Elem.hasValidAttributeQNamesCorrespondingToENames(attributes, simpleScope),
        s"Could not turn all attribute names into QNames (element $name, scope $simpleScope)")

      new Elem(name, attributes, Vector.empty, simpleScope)
    }
  }

}
