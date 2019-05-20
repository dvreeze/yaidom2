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

package eu.cdevreeze.yaidom2.node.jsdom

import java.net.URI

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap
import scala.reflect.classTag

import eu.cdevreeze.yaidom2.core.Declarations
import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.core.UnprefixedName
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.fun.internal.AbstractBackingElemFunctions
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import org.scalajs.dom
import org.scalajs.dom.NamedNodeMap
import org.scalajs.dom.NodeList

/**
 * JS-DOM nodes.
 *
 * @author Chris de Vreeze
 */
// scalastyle:off file.size.limit
object JsDomNodes {

  /**
   * Arbitrary JS-DOM node
   */
  sealed trait Node extends BackingNodes.Node

  /**
   * Potential JS-DOM document child, so an element, processing instruction or comment
   */
  sealed trait CanBeDocumentChild extends Node with BackingNodes.CanBeDocumentChild

  /**
   * JS-DOM element node, offering the `BackingNodes.Elem` element query API.
   */
  // scalastyle:off number.of.methods
  final case class Elem(jsDomElement: dom.Element) extends CanBeDocumentChild with BackingNodes.Elem {

    type ThisElem = Elem

    type ThisNode = Node

    // ElemApi

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterChildElems(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllChildElems(): Seq[ThisElem] = {
      Elem.findAllChildElems(jsDomElement).map(n => Elem(n))
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findChildElem(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterDescendantElems(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllDescendantElems(): Seq[ThisElem] = {
      Elem.findAllDescendantElems(jsDomElement).map(n => Elem(n))
    }

    def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findDescendantElem(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterDescendantElemsOrSelf(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllDescendantElemsOrSelf(): Seq[ThisElem] = {
      Elem.findAllDescendantElemsOrSelf(jsDomElement).map(n => Elem(n))
    }

    def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findDescendantElemOrSelf(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.findTopmostElems(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.findTopmostElemsOrSelf(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findDescendantElemOrSelf(navigationPath: Seq[Int]): Option[ThisElem] = {
      Elem.findDescendantElemOrSelf(jsDomElement, navigationPath).map(n => Elem(n))
    }

    def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = {
      Elem(Elem.getDescendantElemOrSelf(jsDomElement, navigationPath))
    }

    // ClarkElemApi

    def name: EName = {
      Elem.name(jsDomElement)
    }

    def attributes: SeqMap[EName, String] = {
      Elem.attributes(jsDomElement)
    }

    def localName: String = {
      Elem.localName(jsDomElement)
    }

    def namespaceOption: Option[String] = {
      Elem.namespaceOption(jsDomElement)
    }

    def namespaceAsString: String = {
      Elem.namespaceAsString(jsDomElement)
    }

    def attrOption(attributeName: EName): Option[String] = {
      Elem.attrOption(jsDomElement, attributeName)
    }

    def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
      Elem.attrOption(jsDomElement, attributeNamespaceOption, attributeLocalName)
    }

    def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
      Elem.attrOption(jsDomElement, attributeNamespace, attributeLocalName)
    }

    def attrOption(attributeLocalName: String): Option[String] = {
      Elem.attrOption(jsDomElement, attributeLocalName)
    }

    def attr(attributeName: EName): String = {
      Elem.attr(jsDomElement, attributeName)
    }

    def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
      Elem.attr(jsDomElement, attributeNamespaceOption, attributeLocalName)
    }

    def attr(attributeNamespace: String, attributeLocalName: String): String = {
      Elem.attr(jsDomElement, attributeNamespace, attributeLocalName)
    }

    def attr(attributeLocalName: String): String = {
      Elem.attr(jsDomElement, attributeLocalName)
    }

    def text: String = {
      Elem.text(jsDomElement)
    }

    def normalizedText: String = {
      Elem.normalizedText(jsDomElement)
    }

    def trimmedText: String = {
      Elem.trimmedText(jsDomElement)
    }

    // ScopedElemApi

    def scope: Scope = {
      Elem.scope(jsDomElement)
    }

    def qname: QName = {
      Elem.qname(jsDomElement)
    }

    def attributesByQName: SeqMap[QName, String] = {
      Elem.attributesByQName(jsDomElement)
    }

    def textAsQName: QName = {
      Elem.textAsQName(jsDomElement)
    }

    def textAsResolvedQName: EName = {
      Elem.textAsResolvedQName(jsDomElement)
    }

    def attrAsQNameOption(attributeName: EName): Option[QName] = {
      Elem.attrAsQNameOption(jsDomElement, attributeName)
    }

    def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
      Elem.attrAsQNameOption(jsDomElement, attributeNamespaceOption, attributeLocalName)
    }

    def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
      Elem.attrAsQNameOption(jsDomElement, attributeNamespace, attributeLocalName)
    }

    def attrAsQName(attributeName: EName): QName = {
      Elem.attrAsQName(jsDomElement, attributeName)
    }

    def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
      Elem.attrAsQName(jsDomElement, attributeNamespaceOption, attributeLocalName)
    }

    def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
      Elem.attrAsQName(jsDomElement, attributeNamespace, attributeLocalName)
    }

    def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
      Elem.attrAsResolvedQNameOption(jsDomElement, attributeName)
    }

    def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
      Elem.attrAsResolvedQNameOption(jsDomElement, attributeNamespaceOption, attributeLocalName)
    }

    def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
      Elem.attrAsResolvedQNameOption(jsDomElement, attributeNamespace, attributeLocalName)
    }

    def attrAsResolvedQName(attributeName: EName): EName = {
      Elem.attrAsResolvedQName(jsDomElement, attributeName)
    }

    def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
      Elem.attrAsResolvedQName(jsDomElement, attributeNamespaceOption, attributeLocalName)
    }

    def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
      Elem.attrAsResolvedQName(jsDomElement, attributeNamespace, attributeLocalName)
    }

    // BackingElemApi

    def findParentElem(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findParentElem(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findParentElem(): Option[ThisElem] = {
      Elem.findParentElem(jsDomElement).map(n => Elem(n))
    }

    def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterAncestorElems(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllAncestorElems(): Seq[ThisElem] = {
      Elem.findAllAncestorElems(jsDomElement).map(n => Elem(n))
    }

    def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findAncestorElem(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterAncestorElemsOrSelf(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllAncestorElemsOrSelf(): Seq[ThisElem] = {
      Elem.findAllAncestorElemsOrSelf(jsDomElement).map(n => Elem(n))
    }

    def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findAncestorElemOrSelf(jsDomElement, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllPrecedingSiblingElems(): Seq[ThisElem] = {
      Elem.findAllPrecedingSiblingElems(jsDomElement).map(n => Elem(n))
    }

    def ownNavigationPathRelativeToRootElem: Seq[Int] = {
      Elem.ownNavigationPathRelativeToRootElem(jsDomElement)
    }

    def baseUriOption: Option[URI] = {
      Elem.baseUriOption(jsDomElement)
    }

    def baseUri: URI = {
      Elem.baseUri(jsDomElement)
    }

    def docUriOption: Option[URI] = {
      Elem.docUriOption(jsDomElement)
    }

    def docUri: URI = {
      Elem.docUri(jsDomElement)
    }

    def rootElem: ThisElem = {
      Elem(Elem.rootElem(jsDomElement))
    }

    // ClarkNodes.Elem

    def children: Seq[ThisNode] = {
      Elem.children(jsDomElement).flatMap(n => Node.opt(n))
    }

    def select(step: ElemStep[Elem]): Seq[Elem] = {
      // Implemented directly, instead of in terms of Elem.select.
      step(this)
    }
  }

  /**
   * JS-DOM text node
   */
  final case class Text(jsDomText: dom.Text) extends Node with BackingNodes.Text {

    def text: String = {
      jsDomText.data
    }
  }

  /**
   * JS-DOM comment node
   */
  final case class Comment(jsDomComment: dom.Comment) extends CanBeDocumentChild with BackingNodes.Comment {

    def text: String = {
      jsDomComment.data
    }
  }

  /**
   * JS-DOM processing instruction node
   */
  final case class ProcessingInstruction(jsDomProcessingInstruction: dom.ProcessingInstruction) extends CanBeDocumentChild
    with BackingNodes.ProcessingInstruction {

    def target: String = {
      jsDomProcessingInstruction.target
    }

    def data: String = {
      jsDomProcessingInstruction.data
    }
  }

  object Node {

    def opt(jsDomNode: dom.Node): Option[Node] = {
      jsDomNode match {
        case e: dom.Element => Some(Elem(e))
        case t: dom.CDATASection => Some(Text(t))
        case t: dom.Text => Some(Text(t))
        case c: dom.Comment => Some(Comment(c))
        case pi: dom.ProcessingInstruction => Some(ProcessingInstruction(pi))
        case _ => None
      }
    }

    def extractEName(jsDomElement: dom.Element): EName = {
      val nsOption = Option(jsDomElement.namespaceURI)
      EName(nsOption, jsDomElement.localName)
    }

    def extractEName(jsDomAttr: dom.Attr): EName = {
      require(!isNamespaceDeclaration(jsDomAttr), "Namespace declaration not allowed")
      val nsOption = Option(jsDomAttr.namespaceURI)
      EName(nsOption, jsDomAttr.localName)
    }

    def extractQName(jsDomElement: dom.Element): QName = {
      val name: String = jsDomElement.tagName
      val arr = name.split(':')
      assert(arr.length >= 1 && arr.length <= 2)
      if (arr.length == 1) UnprefixedName(arr(0)) else QName(arr(0), arr(1))
    }

    def extractQName(jsDomAttr: dom.Attr): QName = {
      require(!isNamespaceDeclaration(jsDomAttr), "Namespace declaration not allowed")
      val name: String = jsDomAttr.name
      val arr = name.split(':')
      assert(arr.length >= 1 && arr.length <= 2)
      if (arr.length == 1) UnprefixedName(arr(0)) else QName(arr(0), arr(1))
    }

    /** Returns true if the `org.scalajs.dom.Attr` is a namespace declaration */
    def isNamespaceDeclaration(jsDomAttr: dom.Attr): Boolean = {
      val name: String = jsDomAttr.name
      val arr = name.split(':')
      assert(arr.length >= 1 && arr.length <= 2)
      val result = arr(0) == "xmlns"
      result
    }

    /** Helper method that converts a `NodeList` to an `Seq[org.scalajs.dom.Node]` */
    def nodeListToSeq(nodeList: NodeList): Seq[dom.Node] = {
      val result = (0 until nodeList.length).map(i => nodeList.item(i))
      result.to(ArraySeq)
    }

    /** Converts the namespace declarations in a `NamedNodeMap` to a `Declarations` */
    def extractNamespaceDeclarations(domAttributes: NamedNodeMap): Declarations = {
      val nsMap = {
        val result = (0 until domAttributes.length).flatMap { i =>
          val attr = domAttributes.item(i)

          if (isNamespaceDeclaration(attr)) {
            val result = extractNamespaceDeclaration(attr)
            Some(result) map { pair => (pair._1.getOrElse(""), pair._2) }
          } else {
            None
          }
        }
        result.toMap
      }
      Declarations.from(nsMap)
    }

    /** Extracts (optional) prefix and namespace. Call only if `isNamespaceDeclaration(v)`, since otherwise an exception is thrown. */
    def extractNamespaceDeclaration(v: dom.Attr): (Option[String], String) = {
      val name: String = v.name
      val arr = name.split(':')
      assert(arr.length >= 1 && arr.length <= 2)
      require(arr(0) == "xmlns")
      val prefixOption: Option[String] = if (arr.length == 1) None else Some(arr(1))
      val attrValue: String = v.value
      (prefixOption, attrValue)
    }

    private[jsdom] val EmptyUri: URI = URI.create("")
  }

  object CanBeDocumentChild {

    def opt(jsDomNode: dom.Node): Option[CanBeDocumentChild] = {
      jsDomNode match {
        case e: dom.Element => Some(Elem(e))
        case c: dom.Comment => Some(Comment(c))
        case pi: dom.ProcessingInstruction => Some(ProcessingInstruction(pi))
        case _ => None
      }
    }
  }

  object Elem extends AbstractBackingElemFunctions {

    type ElemType = dom.Element

    type NodeType = dom.Node

    protected[yaidom2] def toImmutableSeq(xs: collection.Seq[ElemType]): Seq[ElemType] = {
      ArraySeq.from(xs)(classTag[ElemType])
    }

    def filterChildElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      children(elem).collect { case e: dom.Element if p(e) => e }
    }

    def findChildElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      children(elem).collectFirst { case e: dom.Element if p(e) => e }
    }

    def findDescendantElemOrSelf(elem: ElemType, navigationPath: Seq[Int]): Option[ElemType] = {
      if (navigationPath.isEmpty) {
        Some(elem)
      } else {
        val childElemIdx: Int = navigationPath(0)
        val childElems: Seq[ElemType] = findAllChildElems(elem)

        if (childElemIdx >= 0 && childElemIdx < childElems.size) {
          // Recursive call
          Option(childElems(childElemIdx)).flatMap(che => findDescendantElemOrSelf(che, navigationPath.drop(1)))
        } else {
          None
        }
      }
    }

    def name(elem: ElemType): EName = {
      Node.extractEName(elem)
    }

    def attributes(elem: ElemType): SeqMap[EName, String] = {
      val domAttributes = elem.attributes

      (0 until domAttributes.length).flatMap { i =>
        val attr = domAttributes.item(i)

        if (Node.isNamespaceDeclaration(attr)) {
          None
        } else {
          val ename: EName = Node.extractEName(attr)
          Some(ename -> attr.value)
        }
      }.to(SeqMap)
    }

    def text(elem: ElemType): String = {
      val textStrings = children(elem).collect { case t: dom.Text => t }.map(_.data)
      textStrings.mkString
    }

    def children(elem: ElemType): Seq[NodeType] = {
      Node.nodeListToSeq(elem.childNodes)
    }

    def select(elem: ElemType, step: ElemStep[ElemType]): Seq[ElemType] = {
      step(elem)
    }

    /**
     * Returns the Scope. It is an expensive method for this element implementation.
     */
    def scope(elem: ElemType): Scope = {
      val ancestryOrSelf = findAllAncestorElemsOrSelf(elem)

      val resultScope =
        ancestryOrSelf.foldRight(Scope.Empty) {
          case (wrappedElem, accScope) =>
            val decls = Node.extractNamespaceDeclarations(wrappedElem.attributes)
            accScope.resolve(decls)
        }
      resultScope
    }

    def qname(elem: ElemType): QName = {
      Node.extractQName(elem)
    }

    def attributesByQName(elem: ElemType): SeqMap[QName, String] = {
      val domAttributes = elem.attributes

      (0 until domAttributes.length).flatMap { i =>
        val attr = domAttributes.item(i)

        if (Node.isNamespaceDeclaration(attr)) {
          None
        } else {
          val qname: QName = Node.extractQName(attr)
          Some(qname -> attr.value)
        }
      }.to(SeqMap)
    }

    def findParentElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      Option(elem.parentNode).collect { case e: dom.Element if p(e) => e }
    }

    def findAllPrecedingSiblingElems(elem: ElemType): Seq[ElemType] = {
      @tailrec
      def doFilterPreviousSiblingElements(elem: dom.Element, acc: List[dom.Element]): List[dom.Element] = {
        val prev = elem.previousElementSibling

        if (prev eq null) { // scalastyle:off null
          acc
        } else {
          val newAcc = prev :: acc
          // Recursive call
          doFilterPreviousSiblingElements(prev, newAcc)
        }
      }

      doFilterPreviousSiblingElements(elem, Nil).reverse
    }

    def baseUriOption(elem: ElemType): Option[URI] = {
      // TODO
      docUriOption(elem)
    }

    def docUriOption(elem: ElemType): Option[URI] = {
      Option(elem.ownerDocument).flatMap(d => Option(d.documentURI)).map(u => URI.create(u))
    }

    def rootElem(elem: ElemType): ElemType = {
      // Recursive call

      findParentElem(elem).map(pe => rootElem(pe)).getOrElse(elem)
    }
  }

}
