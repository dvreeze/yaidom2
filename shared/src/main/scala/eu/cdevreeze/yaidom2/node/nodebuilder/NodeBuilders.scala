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
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.core.StableScope
import eu.cdevreeze.yaidom2.creationapi
import eu.cdevreeze.yaidom2.creationapi.ScopedNodeFactories
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractScopedElem
import eu.cdevreeze.yaidom2.updateapi.internal.AbstractUpdatableElem

import scala.collection.immutable.ListMap

/**
 * "Creation DSL" nodes and documents.
 *
 * @author Chris de Vreeze
 */
object NodeBuilders {

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
   * "Creation DSL" element node, offering the `ScopedNodes.Elem` element query API. The "combined stable scope" of all
   * descendant-or-self elements (which exists due to all these scopes being mutually compatible) is stored for efficiency of most
   * "element creation API" methods.
   *
   * On creation it is not validated whether the element tree has no (prefixed) namespace undeclarations, which are disallowed
   * in XML 1.0. This can be fixed afterwards with calls to methods such as usingExtraScopeDeeply, withoutNamespaceUndeclarations or
   * havingSameScopeInDescendantsOrSelf.
   *
   * Implementation note: this class used a ListMap for the attributes instead of VectorMap (via the SeqMap API), due to Scala issue
   * https://github.com/scala/scala/pull/8854.
   */
  final class Elem private[nodebuilder] (
      val qname: QName,
      val attributesByQName: ListMap[QName, String],
      val stableScope: StableScope,
      val children: Vector[Node], // For querying, ArraySeq would be optimal, but not for (functional) updates
      val combinedStableScope: StableScope
  ) extends CanBeDocumentChild
      with AbstractScopedElem
      with AbstractUpdatableElem
      with creationapi.Elem {

    assert(stableScope.isCompatibleSubScopeOf(combinedStableScope)) // Also follows from the next assertion
    assert(
      findAllChildElems
        .map(_.combinedStableScope)
        .distinct
        .foldLeft(stableScope) { case (acc, sc) => acc.appendCompatibleScope(sc) } == combinedStableScope)

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

    def scope: Scope = stableScope.scope

    // Query API methods

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      Vector.from(xs)
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
     * Replaces the children by the given child nodes. Throws an exception if the scopes are not "compatible".
     * This is a rather expensive operation.
     */
    def withChildren(newChildren: Seq[ThisNode]): ThisElem = {
      val newChildElems: Seq[Elem] = newChildren.collect { case e: Elem => e }
      val combinedStableScopesOfChildren: Seq[StableScope] = newChildElems.map(_.combinedStableScope).distinct

      // The code below throws if appending compatibly fails.

      val newCombinedStableScope: StableScope = combinedStableScopesOfChildren.foldLeft(this.stableScope) {
        case (accKnownScope, currScope) =>
          accKnownScope.appendCompatibleScope(currScope)
      }

      new Elem(this.qname, this.attributesByQName, this.stableScope, newChildren.toVector, newCombinedStableScope)
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

    // Element creation API methods (note that method withChildren is shared with UpdatableElemApi)

    def plusChild(child: Node): ThisElem = {
      withChildren(this.children.appended(child))
    }

    def plusChildOption(childOption: Option[Node]): ThisElem = {
      withChildren(this.children.appendedAll(childOption.toSeq))
    }

    def plusChild(index: Int, child: Node): ThisElem = {
      withChildren(this.children.patch(index, Seq(child), 0))
    }

    def plusChildOption(index: Int, childOption: Option[Node]): ThisElem = {
      withChildren(this.children.patch(index, childOption.toSeq, 0))
    }

    def plusChildren(childSeq: Seq[Node]): ThisElem = {
      withChildren(this.children.appendedAll(childSeq))
    }

    def minusChild(index: Int): ThisElem = {
      withChildren(this.children.patch(index, Seq.empty, 1))
    }

    /**
     * Replaces the attributes by the given attributes. Throws an exception if the QNames of the given attributes cannot be resolved.
     */
    def withAttributes(newAttributes: ListMap[QName, String]): ThisElem = {
      new Elem(
        qname,
        newAttributes,
        stableScope,
        children,
        combinedStableScope
      )
    }

    def plusAttribute(attrQName: QName, attrValue: String): ThisElem = {
      withAttributes(attributesByQName.updated(attrQName, attrValue))
    }

    def plusAttributeOption(attrQName: QName, attrValueOption: Option[String]): ThisElem = {
      attrValueOption.map(v => plusAttribute(attrQName, v)).getOrElse(this)
    }

    def plusAttributes(newAttributes: ListMap[QName, String]): ThisElem = {
      withAttributes(attributesByQName.concat(newAttributes))
    }

    def minusAttribute(attrQName: QName): ThisElem = {
      withAttributes(attributesByQName.filterNot(_._1 == attrQName))
    }

    /**
     * Replaces the element QName by the given QName. Throws an exception if the new QName cannot be resolved.
     */
    def withQName(newQName: QName): ThisElem = {
      new Elem(
        newQName,
        attributesByQName,
        stableScope,
        children,
        combinedStableScope
      )
    }

    /**
     * Appends the given extra stable scope to this element's stable scope, and makes sure there are no namespace undeclarations
     * in the descendant elements. If the extra stable scope cannot be added as a non-conflicting scope to this element
     * or any of its descendants, an exception is thrown.
     *
     * This method is typically used to introduce one or more prefixes and corresponding namespaces to an element and
     * all its descendants.
     *
     * The "combined stable scope" of the result element is `this.combinedStableScope.appendNonConflictingScope(extraScope)`.
     * Hence it is trivial to deduce that the result element is internally consistent.
     */
    def usingExtraScopeDeeply(extraScope: StableScope): ThisElem = {
      // Throws if unsafely appending fails
      val newElemScope: StableScope = this.stableScope.appendNonConflictingScope(extraScope)
      // We already know the new combined stable scope, without having to inspect the descdendant-or-self elements
      val newCombinedElemScope: StableScope = this.combinedStableScope.appendNonConflictingScope(extraScope)

      val newChildNodes: Seq[Node] =
        children.collect {
          case che: NodeBuilders.Elem =>
            // Recursive call
            che.usingExtraScopeDeeply(newElemScope)
          case n => n
        }

      new Elem(this.qname, this.attributesByQName, newElemScope, newChildNodes.toVector, newCombinedElemScope)
    }

    def withExtraScope(extraScope: StableScope): ThisElem = {
      new Elem(
        qname,
        attributesByQName,
        stableScope.appendNonConflictingScope(extraScope),
        children,
        combinedStableScope.appendNonConflictingScope(extraScope)
      )
    }

    def withNeededExtraScope(qnames: Set[QName], knownScope: StableScope): ThisElem = {
      withExtraScope(StableScope.neededCompatibleSubScope(qnames, knownScope))
    }

    // Other public methods

    def attributeScope: Scope = scope.withoutDefaultNamespace

    /**
     * Returns an equivalent Elem, without any namespace undeclarations.
     * That is, returns `usingExtraScopeDeeply(StableScope.empty)`.
     */
    def withoutNamespaceUndeclarations: ThisElem = {
      usingExtraScopeDeeply(StableScope.empty)
    }

    /**
     * Returns an equivalent Elem, without any namespace (un)declarations in the descendants.
     * That is, returns `usingExtraScopeDeeply(elem.combinedStableScope)`.
     */
    def havingSameScopeInDescendantsOrSelf: ThisElem = {
      usingExtraScopeDeeply(this.combinedStableScope)
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

  // Companion objects for Node and Elem

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

    def unapply(elem: Elem): Option[(QName, ListMap[QName, String], Seq[Node], StableScope)] = {
      val v = (elem.qname, elem.attributesByQName, elem.children, elem.stableScope)
      Some(v)
    }

    /**
     * Returns an optional copy of the passed element as "creation DSL" element.
     * If the passed element does not meet the requirements of "creation DSL" elements, None is returned.
     * That is, if the created element would not have a "combinedStableScope", None is returned.
     * The absence of prefixed namespace undeclarations is not checked.
     */
    def optionallyFrom(elm: ScopedNodes.Elem): Option[Elem] = {
      val allElemsOrSelf = elm.findAllDescendantElemsOrSelf

      val allScopes: Seq[Scope] = allElemsOrSelf.map(_.scope).distinct
      val allStableScopes: Seq[StableScope] = allScopes.flatMap(StableScope.optionallyFrom)

      if (allStableScopes.size < allScopes.size) {
        // Not all elements have a stable scope (invertible scope when ignoring the default namespace)
        None
      } else {
        val combinedStableScopeOption: Option[StableScope] = scala.util.Try {
          allStableScopes.tail.foldLeft(allStableScopes.head) {
            case (accScope, currScope) =>
              accScope.appendCompatibleScope(currScope)
          }
        }.toOption

        combinedStableScopeOption.map(_ => from(elm))
      }
    }

    /**
     * Returns a copy of the passed element as "creation DSL" element.
     * If the passed element does not meet the requirements of "creation DSL" elements, an exception is thrown.
     * That is, if the created element would not have a "combinedStableScope", an exception is thrown.
     * The absence of prefixed namespace undeclarations is not checked.
     */
    def from(elm: ScopedNodes.Elem): Elem = {
      val stableScope: StableScope = StableScope.from(elm.scope) // May throw

      val children = elm.children.collect {
        case childElm: ScopedNodes.Elem                 => childElm
        case childText: ScopedNodes.Text                => childText
        case childComment: ScopedNodes.Comment          => childComment
        case childPi: ScopedNodes.ProcessingInstruction => childPi
      }

      // Recursion, with Node.from and Elem.from being mutually dependent
      val creationDslChildren = children.map(Node.from)

      // May throw
      val combinedStableScope: StableScope =
        creationDslChildren.collect { case e: Elem => e }.map(_.combinedStableScope).foldLeft(stableScope) {
          case (accScope, sc) => accScope.appendCompatibleScope(sc)
        }

      new Elem(elm.qname, elm.attributesByQName, stableScope, creationDslChildren.to(Vector), combinedStableScope)
    }
  }
}
