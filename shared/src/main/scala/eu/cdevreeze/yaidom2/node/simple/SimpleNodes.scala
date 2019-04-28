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
import scala.collection.mutable

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.oo.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.oofun.ScopedElemFunctionWrapper

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
  // scalastyle:off number.of.methods
  final class Elem(
    val qname: QName,
    val attributesByQName: SeqMap[QName, String],
    val scope: Scope,
    val children: ArraySeq[Node]
  ) extends CanBeDocumentChild with ScopedNodes.Elem {

    // TODO Requirements on constructor parameters

    type ThisElem = Elem

    type ThisNode = Node

    // ElemApi

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e@Elem(_, _, _, _) if p(e) => e }
    }

    def findAllChildElems(): Seq[ThisElem] = {
      filterChildElems(_ => true)
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e@Elem(_, _, _, _) if p(e) => e }
    }

    def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      findAllChildElems().flatMap(_.filterDescendantElemsOrSelf(p))
    }

    def findAllDescendantElems(): Seq[ThisElem] = {
      filterDescendantElems(_ => true)
    }

    def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
      findAllChildElems().view.flatMap(_.findDescendantElemOrSelf(p)).headOption
    }

    def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      val result = mutable.ArrayBuffer[ThisElem]()

      def accumulate(elm: ThisElem): Unit = {
        if (p(elm)) result += elm
        // Recursive calls (not tail-recursive, but the depth is typically limited)
        elm.findAllChildElems().foreach(accumulate)
      }

      accumulate(this)
      result.to(ArraySeq)
    }

    def findAllDescendantElemsOrSelf(): Seq[ThisElem] = {
      filterDescendantElemsOrSelf(_ => true)
    }

    def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      var result: Option[ThisElem] = None

      def findElem(elm: ThisElem): Unit = {
        if (result.isEmpty) {
          if (p(elm)) result = Some(elm)
        }
        if (result.isEmpty) {
          // Recursive calls (not tail-recursive, but the depth is typically limited)
          elm.findAllChildElems().foreach(findElem)
        }
      }

      findElem(this)
      result
    }

    def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      findAllChildElems().flatMap(_.findTopmostElemsOrSelf(p))
    }

    def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      val result = mutable.ArrayBuffer[ThisElem]()

      def accumulate(elm: ThisElem): Unit = {
        if (p(elm)) {
          result += elm
        } else {
          // Recursive calls (not tail-recursive, but the depth is typically limited)
          elm.findAllChildElems().foreach(accumulate)
        }
      }

      accumulate(this)
      result.to(ArraySeq)
    }

    // ClarkElemApi

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

    def localName: String = {
      name.localPart
    }

    def namespaceOption: Option[String] = {
      name.namespaceUriOption
    }

    def namespaceAsString: String = {
      namespaceOption.getOrElse("")
    }

    def attrOption(attributeName: EName): Option[String] = {
      attributes.get(attributeName)
    }

    def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
      attrOption(EName(attributeNamespaceOption, attributeLocalName))
    }

    def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
      attrOption(EName(Some(attributeNamespace), attributeLocalName))
    }

    def attrOption(attributeLocalName: String): Option[String] = {
      attrOption(EName(None, attributeLocalName))
    }

    def attr(attributeName: EName): String = {
      attrOption(attributeName).get
    }

    def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
      attrOption(attributeNamespaceOption, attributeLocalName).get
    }

    def attr(attributeNamespace: String, attributeLocalName: String): String = {
      attrOption(attributeNamespace, attributeLocalName).get
    }

    def attr(attributeLocalName: String): String = {
      attrOption(attributeLocalName).get
    }

    def text: String = {
      children.collect { case Text(t, _) => t }.mkString
    }

    def normalizedText: String = {
      normalizeString(text)
    }

    def trimmedText: String = {
      text.trim
    }

    // ScopedElemApi

    def textAsQName: QName = {
      QName.parse(text.trim)
    }

    def textAsResolvedQName: EName = {
      scope.resolveQNameOption(textAsQName).getOrElse(
        sys.error(s"Could not resolve QName-valued element text $textAsQName, given scope [$scope]"))
    }

    def attrAsQNameOption(attributeName: EName): Option[QName] = {
      attrOption(attributeName).map(v => QName.parse(v.trim))
    }

    def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
      attrOption(attributeNamespaceOption, attributeLocalName).map(v => QName.parse(v.trim))
    }

    def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
      attrOption(attributeNamespace, attributeLocalName).map(v => QName.parse(v.trim))
    }

    def attrAsQName(attributeName: EName): QName = {
      attrAsQNameOption(attributeName).getOrElse(
        sys.error(s"Missing QName-valued attribute $attributeName"))
    }

    def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
      attrAsQNameOption(attributeNamespaceOption, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
    }

    def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
      attrAsQNameOption(attributeNamespace, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
    }

    def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
      attrAsQNameOption(attributeName).map { qn =>
        scope.resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [$scope]"))
      }
    }

    def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
      attrAsQNameOption(attributeNamespaceOption, attributeLocalName).map { qn =>
        scope.resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [$scope]"))
      }
    }

    def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
      attrAsQNameOption(attributeNamespace, attributeLocalName).map { qn =>
        scope.resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [$scope]"))
      }
    }

    def attrAsResolvedQName(attributeName: EName): EName = {
      attrAsResolvedQNameOption(attributeName).getOrElse(
        sys.error(s"Missing QName-valued attribute $attributeName"))
    }

    def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
      attrAsResolvedQNameOption(attributeNamespaceOption, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
    }

    def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
      attrAsResolvedQNameOption(attributeNamespace, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
    }

    // ClarkNodes.Elem

    def select(step: ElemStep[Elem]): Seq[Elem] = {
      step(this)
    }

    // Other public methods

    def attributeScope: Scope = scope.withoutDefaultNamespace

    /**
     * Returns the element found by navigating the given element navigation path starting with this element.
     * If no element exists at the given navigation path, an exception is thrown.
     *
     * Note that each step in the navigation path is a zero-based child element index, not any child node index!
     */
    def atNavigationPath(navigationPath: Seq[Int]): Elem = {
      if (navigationPath.isEmpty) {
        this
      } else {
        val childElem: Elem = findAllChildElems().apply(navigationPath.head)

        // Recursive call
        childElem.atNavigationPath(navigationPath.tail)
      }
    }

    // Private methods

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

  object Node {

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

  object Elem extends ScopedElemFunctionWrapper {

    type ElemType = Elem

    type NodeType = Node

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
