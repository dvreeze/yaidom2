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
import scala.collection.mutable

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.creationapi.BackingNodeConverters
import eu.cdevreeze.yaidom2.node.simple.SimpleNodes
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
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
  // scalastyle:off number.of.methods
  final class Elem private(
    val docUriOption: Option[URI],
    val underlyingRootElem: SimpleNodes.Elem,
    val elemNavigationPathFromRoot: ArraySeq[Int],
    val underlyingElem: SimpleNodes.Elem
  ) extends CanBeDocumentChild with BackingNodes.Elem {

    import Elem.emptyUri
    import Elem.XmlBaseEName

    type ThisElem = Elem

    type ThisNode = Node

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

    // ElemApi

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      underlyingElem.findAllChildElems().zipWithIndex
        .map { case (e, idx) =>
          new Elem(docUriOption, underlyingRootElem, elemNavigationPathFromRoot.appended(idx), e)
        }.filter(p)
    }

    def findAllChildElems(): Seq[ThisElem] = {
      filterChildElems(_ => true)
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      filterChildElems(p).headOption
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
      underlyingElem.name
    }

    def attributes: SeqMap[EName, String] = {
      underlyingElem.attributes
    }

    def localName: String = {
      underlyingElem.localName
    }

    def namespaceOption: Option[String] = {
      underlyingElem.namespaceOption
    }

    def namespaceAsString: String = {
      underlyingElem.namespaceAsString
    }

    def attrOption(attributeName: EName): Option[String] = {
      underlyingElem.attrOption(attributeName)
    }

    def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
      underlyingElem.attrOption(attributeNamespaceOption, attributeLocalName)
    }

    def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
      underlyingElem.attrOption(attributeNamespace, attributeLocalName)
    }

    def attrOption(attributeLocalName: String): Option[String] = {
      underlyingElem.attrOption(attributeLocalName)
    }

    def attr(attributeName: EName): String = {
      underlyingElem.attr(attributeName)
    }

    def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
      underlyingElem.attr(attributeNamespaceOption, attributeLocalName)
    }

    def attr(attributeNamespace: String, attributeLocalName: String): String = {
      underlyingElem.attr(attributeNamespace, attributeLocalName)
    }

    def attr(attributeLocalName: String): String = {
      underlyingElem.attr(attributeLocalName)
    }

    def text: String = {
      underlyingElem.text
    }

    def normalizedText: String = {
      underlyingElem.normalizedText
    }

    def trimmedText: String = {
      underlyingElem.trimmedText
    }

    // ScopedElemApi

    def scope: Scope = {
      underlyingElem.scope
    }

    def qname: QName = {
      underlyingElem.qname
    }

    def attributesByQName: SeqMap[QName, String] = {
      underlyingElem.attributesByQName
    }

    def textAsQName: QName = {
      underlyingElem.textAsQName
    }

    def textAsResolvedQName: EName = {
      underlyingElem.textAsResolvedQName
    }

    def attrAsQNameOption(attributeName: EName): Option[QName] = {
      underlyingElem.attrAsQNameOption(attributeName)
    }

    def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
      underlyingElem.attrAsQNameOption(attributeNamespaceOption, attributeLocalName)
    }

    def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
      underlyingElem.attrAsQNameOption(attributeNamespace, attributeLocalName)
    }

    def attrAsQName(attributeName: EName): QName = {
      underlyingElem.attrAsQName(attributeName)
    }

    def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
      underlyingElem.attrAsQName(attributeNamespaceOption, attributeLocalName)
    }

    def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
      underlyingElem.attrAsQName(attributeNamespace, attributeLocalName)
    }

    def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
      underlyingElem.attrAsResolvedQNameOption(attributeName)
    }

    def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
      underlyingElem.attrAsResolvedQNameOption(attributeNamespaceOption, attributeLocalName)
    }

    def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
      underlyingElem.attrAsResolvedQNameOption(attributeNamespace, attributeLocalName)
    }

    def attrAsResolvedQName(attributeName: EName): EName = {
      underlyingElem.attrAsResolvedQName(attributeName)
    }

    def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
      underlyingElem.attrAsResolvedQName(attributeNamespaceOption, attributeLocalName)
    }

    def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
      underlyingElem.attrAsResolvedQName(attributeNamespace, attributeLocalName)
    }

    // BackingElemApi

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

    def findParentElem(): Option[ThisElem] = {
      findParentElem(_ => true)
    }

    def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      findParentElem().to(ArraySeq).flatMap(_.filterAncestorElemsOrSelf(p))
    }

    def findAllAncestorElems(): Seq[ThisElem] = {
      filterAncestorElems(_ => true)
    }

    def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = {
      filterAncestorElems(p).headOption // TODO Improve performance!
    }

    def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      // Recursive calls
      ArraySeq(this).filter(p) ++ findParentElem().to(ArraySeq).flatMap(_.filterAncestorElemsOrSelf(p))
    }

    def findAllAncestorElemsOrSelf(): Seq[ThisElem] = {
      filterAncestorElemsOrSelf(_ => true)
    }

    def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      filterAncestorElemsOrSelf(p).headOption // TODO Improve performance!
    }

    def findAllPrecedingSiblingElems(): Seq[ThisElem] = {
      val parentElemOption = findParentElem()

      if (parentElemOption.isEmpty) {
        ArraySeq.empty
      } else {
        parentElemOption.get.findAllChildElems().takeWhile(_ != Elem.this).reverse
      }
    }

    def baseUriOption: Option[URI] = {
      // Recursive call
      val parentBaseUriOption: Option[URI] =
        findParentElem().flatMap(_.baseUriOption).orElse(docUriOption)

      attrOption(XmlBaseEName).map(u => URI.create(u))
        .map(u => parentBaseUriOption.map(_.resolve(u)).getOrElse(u)).orElse(parentBaseUriOption)
    }

    def baseUri: URI = {
      baseUriOption.getOrElse(emptyUri)
    }

    def docUri: URI = {
      docUriOption.getOrElse(emptyUri)
    }

    def rootElem: ThisElem = {
      new Elem(docUriOption, underlyingRootElem, ArraySeq.empty, underlyingRootElem)
    }

    // ClarkNodes.Elem

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

    def select(step: ElemStep[Elem]): Seq[Elem] = {
      step(this)
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
        val parentElemOption = elm.findParentElem()

        if (parentElemOption.isEmpty) {
          Nil
        } else {
          // Recursive call
          elm.findAllPrecedingSiblingElems().size :: computeReversePath(parentElemOption.get)
        }
      }

      computeReversePath(elm).reverse.to(ArraySeq)
    }

    private val emptyUri = URI.create("")

    private val XmlBaseEName = EName("http://www.w3.org/XML/1998/namespace", "base")
  }

}
