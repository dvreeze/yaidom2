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
import scala.collection.mutable

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.fun.ScopedElemFunctionsApi
import eu.cdevreeze.yaidom2.queryapi.oo.ScopedNodes

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
    val attributesByQName: Seq[(QName, String)],
    val scope: Scope,
    val children: Seq[Node]
  ) extends CanBeDocumentChild with ScopedNodes.Elem {

    // TODO Requirements on constructor parameters

    type ThisElem = Elem

    type ThisNode = Node

    // ElemApi

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e@Elem(_, _, _, _) if p(e) => e }
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e@Elem(_, _, _, _) if p(e) => e }
    }

    def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      filterChildElems(_ => true).flatMap(_.filterDescendantElemsOrSelf(p))
    }

    def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
      filterChildElems(_ => true).view.flatMap(_.findDescendantElemOrSelf(p)).headOption
    }

    def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      val result = mutable.ArrayBuffer[ThisElem]()

      def accumulate(elm: ThisElem): Unit = {
        if (p(elm)) result += elm
        // Recursive calls (not tail-recursive, but the depth is typically limited)
        elm.filterChildElems(_ => true).foreach(accumulate)
      }

      accumulate(this)
      result.to(ArraySeq)
    }

    def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      var result: Option[ThisElem] = None

      def findElem(elm: ThisElem): Unit = {
        if (result.isEmpty) {
          if (p(elm)) result = Some(elm)
        }
        if (result.isEmpty) {
          // Recursive calls (not tail-recursive, but the depth is typically limited)
          elm.filterChildElems(_ => true).foreach(findElem)
        }
      }

      findElem(this)
      result
    }

    def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      filterChildElems(_ => true).flatMap(_.findTopmostElemsOrSelf(p))
    }

    def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      val result = mutable.ArrayBuffer[ThisElem]()

      def accumulate(elm: ThisElem): Unit = {
        if (p(elm)) {
          result += elm
        } else {
          // Recursive calls (not tail-recursive, but the depth is typically limited)
          elm.filterChildElems(_ => true).foreach(accumulate)
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

    def attributes: Seq[(EName, String)] = {
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
      attributes.toMap.get(attributeName) // TODO Improve performance
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

    type NodeType = Node

    type ElemType = Elem

    def from(node: ScopedNodes.Node): Node = node match {
      case e: ScopedNodes.Elem => Elem.from(e)
      case t: ScopedNodes.Text => Text(t.text, false)
      case c: ScopedNodes.Comment => Comment(c.text)
      case pi: ScopedNodes.ProcessingInstruction => ProcessingInstruction(pi.target, pi.data)
      case n => sys.error(s"Not an element, text, comment or processing instruction node: $n")
    }
  }

  // scalastyle:off number.of.methods
  object Elem extends ScopedElemFunctionsApi {

    type ElemType = Elem

    type NodeType = Node

    def unapply(elem: Elem): Option[(QName, Seq[(QName, String)], Scope, Seq[Node])] = {
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

      new Elem(elm.qname, elm.attributesByQName.toSeq, elm.scope, simpleChildren)
    }

    def filterChildElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      elem.filterChildElems(p)
    }

    def findChildElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      elem.findChildElem(p)
    }

    def filterDescendantElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      elem.filterDescendantElems(p)
    }

    def findDescendantElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      elem.findDescendantElem(p)
    }

    def filterDescendantElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      elem.filterDescendantElemsOrSelf(p)
    }

    def findDescendantElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      elem.findDescendantElemOrSelf(p)
    }

    def findTopmostElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      elem.findTopmostElems(p)
    }

    def findTopmostElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      elem.findTopmostElemsOrSelf(p)
    }

    def name(elem: ElemType): EName = {
      elem.name
    }

    def attributes(elem: ElemType): Seq[(EName, String)] = {
      elem.attributes
    }

    def localName(elem: ElemType): String = {
      elem.localName
    }

    def namespaceOption(elem: ElemType): Option[String] = {
      elem.namespaceOption
    }

    def namespaceAsString(elem: ElemType): String = {
      elem.namespaceAsString
    }

    def attrOption(elem: ElemType, attributeName: EName): Option[String] = {
      elem.attrOption(attributeName)
    }

    def attrOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
      elem.attrOption(attributeNamespaceOption, attributeLocalName)
    }

    def attrOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[String] = {
      elem.attrOption(attributeNamespace, attributeLocalName)
    }

    def attrOption(elem: ElemType, attributeLocalName: String): Option[String] = {
      elem.attrOption(attributeLocalName)
    }

    def attr(elem: ElemType, attributeName: EName): String = {
      elem.attr(attributeName)
    }

    def attr(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
      elem.attr(attributeNamespaceOption, attributeLocalName)
    }

    def attr(elem: ElemType, attributeNamespace: String, attributeLocalName: String): String = {
      elem.attr(attributeNamespace, attributeLocalName)
    }

    def attr(elem: ElemType, attributeLocalName: String): String = {
      elem.attr(attributeLocalName)
    }

    def text(elem: ElemType): String = {
      elem.text
    }

    def normalizedText(elem: ElemType): String = {
      elem.normalizedText
    }

    def trimmedText(elem: ElemType): String = {
      elem.trimmedText
    }

    def children(elem: ElemType): Seq[NodeType] = {
      elem.children
    }

    def select(elem: ElemType, step: ElemStep[ElemType]): Seq[ElemType] = {
      elem.select(step)
    }

    def scope(elem: ElemType): Scope = {
      elem.scope
    }

    def qname(elem: ElemType): QName = {
      elem.qname
    }

    def attributesByQName(elem: ElemType): Seq[(QName, String)] = {
      elem.attributesByQName
    }

    def textAsQName(elem: ElemType): QName = {
      elem.textAsQName
    }

    def textAsResolvedQName(elem: ElemType): EName = {
      elem.textAsResolvedQName
    }

    def attrAsQNameOption(elem: ElemType, attributeName: EName): Option[QName] = {
      elem.attrAsQNameOption(attributeName)
    }

    def attrAsQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
      elem.attrAsQNameOption(attributeNamespaceOption, attributeLocalName)
    }

    def attrAsQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[QName] = {
      elem.attrAsQNameOption(attributeNamespace, attributeLocalName)
    }

    def attrAsQName(elem: ElemType, attributeName: EName): QName = {
      elem.attrAsQName(attributeName)
    }

    def attrAsQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
      elem.attrAsQName(attributeNamespaceOption, attributeLocalName)
    }

    def attrAsQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): QName = {
      elem.attrAsQName(attributeNamespace, attributeLocalName)
    }

    def attrAsResolvedQNameOption(elem: ElemType, attributeName: EName): Option[EName] = {
      elem.attrAsResolvedQNameOption(attributeName)
    }

    def attrAsResolvedQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
      elem.attrAsResolvedQNameOption(attributeNamespaceOption, attributeLocalName)
    }

    def attrAsResolvedQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[EName] = {
      elem.attrAsResolvedQNameOption(attributeNamespace, attributeLocalName)
    }

    def attrAsResolvedQName(elem: ElemType, attributeName: EName): EName = {
      elem.attrAsResolvedQName(attributeName)
    }

    def attrAsResolvedQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
      elem.attrAsResolvedQName(attributeNamespaceOption, attributeLocalName)
    }

    def attrAsResolvedQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): EName = {
      elem.attrAsResolvedQName(attributeNamespace, attributeLocalName)
    }
  }

}
