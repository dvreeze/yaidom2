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
package eu.cdevreeze.yaidom2.node.saxon

import java.net.URI

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.XdmNodeKind

/**
 * Saxon-backed nodes.
 *
 * @author Chris de Vreeze
 */
object SaxonNodes {

  /**
   * Arbitrary Saxon node
   */
  sealed trait Node extends Any with BackingNodes.Node

  /**
   * Potential Saxon document child, so an element, processing instruction or comment
   */
  sealed trait CanBeDocumentChild extends Any with Node with BackingNodes.CanBeDocumentChild

  /**
   * Saxon element node, offering the `BackingElemApi with HasChildNodesApi` element query API.
   * Note that this is a value class, so no object creation is done for these "wrapper elements".
   */
  final case class Elem(xdmNode: XdmNode) extends AnyVal with CanBeDocumentChild with BackingNodes.Elem {

    type ThisElem = Elem

    type ThisNode = Node

    // ElemApi

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      ???
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      ???
    }

    def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      ???
    }

    def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
      ???
    }

    def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      ???
    }

    def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      ???
    }

    def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      ???
    }

    def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      ???
    }

    // ClarkElemApi

    def name: EName = {
      ???
    }

    def attributes: Iterable[(EName, String)] = {
      ???
    }

    def localName: String = {
      ???
    }

    def attrOption(attributeName: EName): Option[String] = {
      ???
    }

    def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
      ???
    }

    def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
      ???
    }

    def attrOption(attributeLocalName: String): Option[String] = {
      ???
    }

    def attr(attributeName: EName): String = {
      ???
    }

    def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
      ???
    }

    def attr(attributeNamespace: String, attributeLocalName: String): String = {
      ???
    }

    def text: String = {
      ???
    }

    def normalizedText: String = {
      ???
    }

    def trimmedText: String = {
      ???
    }

    // ScopedElemApi

    def scope: Scope = {
      ???
    }

    def qname: QName = {
      ???
    }

    def attributesByQName: Iterable[(QName, String)] = {
      ???
    }

    def textAsQName: QName = {
      ???
    }

    def textAsResolvedQName: EName = {
      ???
    }

    def attrAsQNameOption(attributeName: EName): Option[QName] = {
      ???
    }

    def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
      ???
    }

    def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
      ???
    }

    def attrAsQName(attributeName: EName): QName = {
      ???
    }

    def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
      ???
    }

    def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
      ???
    }

    def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
      ???
    }

    def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
      ???
    }

    def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
      ???
    }

    def attrAsResolvedQName(attributeName: EName): EName = {
      ???
    }

    def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
      ???
    }

    def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
      ???
    }

    // BackingElemApi

    def findParentElem(p: ThisElem => Boolean): Option[ThisElem] = {
      ???
    }

    def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      ???
    }

    def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = {
      ???
    }

    def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      ???
    }

    def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      ???
    }

    def baseUriOption: Option[URI] = {
      ???
    }

    def baseUri: URI = {
      ???
    }

    def docUriOption: Option[URI] = {
      ???
    }

    def docUri: URI = {
      ???
    }

    def rootElem: ThisElem = {
      ???
    }

    // ClarkNodes.Elem

    def children: IndexedSeq[ThisNode] = {
      ???
    }
  }

  /**
   * Saxon text node
   */
  final case class Text(xdmNode: XdmNode) extends AnyVal with Node with BackingNodes.Text {

    def text: String = {
      xdmNode.getUnderlyingNode.getStringValue
    }

    def validate(): Unit = {
      require(xdmNode.getNodeKind == XdmNodeKind.TEXT, s"Not a text node: $xdmNode")
    }
  }

  /**
   * Saxon comment node
   */
  final case class Comment(xdmNode: XdmNode) extends AnyVal with CanBeDocumentChild with BackingNodes.Comment {

    def text: String = {
      xdmNode.getUnderlyingNode.getStringValue
    }

    def validate(): Unit = {
      require(xdmNode.getNodeKind == XdmNodeKind.COMMENT, s"Not a comment node: $xdmNode")
    }
  }

  /**
   * Saxon processing instruction node
   */
  final case class ProcessingInstruction(xdmNode: XdmNode) extends AnyVal with CanBeDocumentChild with BackingNodes.ProcessingInstruction {

    def target: String = {
      xdmNode.getUnderlyingNode.getDisplayName
    }

    def data: String = {
      xdmNode.getUnderlyingNode.getStringValue
    }

    def validate(): Unit = {
      require(xdmNode.getNodeKind == XdmNodeKind.PROCESSING_INSTRUCTION, s"Not a processing instruction: $xdmNode")
    }
  }
}
