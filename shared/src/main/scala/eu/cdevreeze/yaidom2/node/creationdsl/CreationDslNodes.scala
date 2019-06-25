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

package eu.cdevreeze.yaidom2.node.creationdsl

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
 * "Creation DSL" nodes.
 *
 * @author Chris de Vreeze
 */
object CreationDslNodes {

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
  final class Elem private(
    val name: EName,
    val attributes: SeqMap[EName, String],
    val children: ArraySeq[Node],
    val scope: Scope
  ) extends CanBeDocumentChild with AbstractScopedElem with AbstractUpdatableElem {

    assert(scope.isInvertible)
    assert(scope.defaultNamespaceOption.isEmpty)

    type ThisElem = Elem

    type ThisNode = Node

    // Query API methods

    protected[yaidom2] def self: Elem = this

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[Elem]): Seq[Elem] = {
      ArraySeq.from(xs)(classTag[Elem])
    }

    def qname: QName = {
      val prefixOption: Option[String] =
        if (name.namespaceUriOption.isEmpty) None else findPrefixForNamespace(name.namespaceUriOption.get)

      QName(prefixOption, name.localPart)
    }

    def attributesByQName: SeqMap[QName, String] = {
      attributes.map { case (attrName, attrValue) =>
        val prefixOption: Option[String] =
          if (attrName.namespaceUriOption.isEmpty) None else findPrefixForNamespace(attrName.namespaceUriOption.get)

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

    def findAllChildNodes: Seq[ThisNode] = children

    def withChildren(newChildren: Seq[ThisNode]): ThisElem = {
      require(
        newChildren.collect { case e: Elem => e }.forall(e => scope.subScopeOf(e.scope)),
        s"Not all child elements have a strict super-scope of $scope")

      new Elem(name, attributes, newChildren.to(ArraySeq), scope)
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

    // Other methods

    def deeplyEnhancingScopeWith(extraScope: Scope): Elem = {
      require(extraScope.isInvertible, s"Not an invertible scope $extraScope")
      require(extraScope.defaultNamespaceOption.isEmpty, s"Not a scope having no default namespace $extraScope")

      transformDescendantElemsOrSelf { e =>
        require(e.scope.append(extraScope).isInvertible, s"Not an invertible result scope ${e.scope.append(extraScope)}")
        require(e.scope.subScopeOf(e.scope.append(extraScope)), s"Not a subscope. Scope 1: ${e.scope}. Scope 2: ${e.scope.append(extraScope)}")

        new Elem(e.name, e.attributes, e.children, e.scope.append(extraScope))
      }
    }

    // Private methods

    private def findPrefixForNamespace(namespace: String): Option[String] = {
      if (namespace == Scope.XmlNamespace) {
        Some("xml")
      } else {
        scope.prefixesForNamespace(namespace).headOption
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

    // TODO Unapply

    def from(elm: ScopedNodes.Elem): Elem = {
      require(elm.scope.isInvertible, s"Not an invertible scope: ${elm.scope}")
      require(elm.scope.defaultNamespaceOption.isEmpty, s"Not a scope without default namespace: ${elm.scope}")

      val children = elm.children.collect {
        case childElm: ScopedNodes.Elem =>
          require(elm.scope.subScopeOf(childElm.scope), s"Scope ${elm.scope} is not a subscope of ${childElm.scope}")

          childElm
        case childText: ScopedNodes.Text =>
          childText
      }

      // Recursion, with Node.from and Elem.from being mutually dependent
      val creationDslChildren = children.map { node => Node.from(node) }

      new Elem(elm.name, elm.attributes, creationDslChildren.to(ArraySeq), elm.scope)
    }
  }

}
