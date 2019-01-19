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

import scala.collection.JavaConverters._
import scala.collection.immutable.ArraySeq
import scala.compat.java8.OptionConverters._

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.XdmNodeKind
import net.sf.saxon.s9api.streams.Predicates._
import net.sf.saxon.s9api.streams.Steps._

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
      validate()

      val stream = xdmNode.select(child(isElement).where(n => p(Elem(n))))
      stream.asListOfNodes.asScala.to(ArraySeq).map(n => Elem(n))
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      validate()

      val stream = xdmNode.select(child(isElement).where(n => p(Elem(n))))
      stream.asOptionalNode.asScala.map(n => Elem(n))
    }

    def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      validate()

      val stream = xdmNode.select(descendant(isElement).where(n => p(Elem(n))))
      stream.asListOfNodes.asScala.to(ArraySeq).map(n => Elem(n))
    }

    def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
      validate()

      val stream = xdmNode.select(descendant(isElement).where(n => p(Elem(n))))
      stream.asOptionalNode.asScala.map(n => Elem(n))
    }

    def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      validate()

      val stream = xdmNode.select(descendantOrSelf(isElement).where(n => p(Elem(n))))
      stream.asListOfNodes.asScala.to(ArraySeq).map(n => Elem(n))
    }

    def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      validate()

      val stream = xdmNode.select(descendantOrSelf(isElement).where(n => p(Elem(n))))
      stream.asOptionalNode.asScala.map(n => Elem(n))
    }

    def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      validate()

      // TODO
      ???
    }

    def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      validate()

      // TODO
      ???
    }

    // ClarkElemApi

    def name: EName = {
      validate()

      Node.extractEName(xdmNode)
    }

    def attributes: Iterable[(EName, String)] = {
      validate()

      val stream = xdmNode.select(attribute())
      stream.asListOfNodes.asScala.to(ArraySeq).map(n => Node.extractEName(n) -> n.getStringValue)
    }

    def localName: String = {
      validate()

      xdmNode.getUnderlyingNode.getLocalPart
    }

    def attrOption(attributeName: EName): Option[String] = {
      validate()

      val stream = xdmNode.select(attribute(attributeName.namespaceUriOption.getOrElse(""), attributeName.localPart))
      stream.asOptionalNode.asScala.map(_.getStringValue)
    }

    def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
      validate()

      val stream = xdmNode.select(attribute(attributeNamespaceOption.getOrElse(""), attributeLocalName))
      stream.asOptionalNode.asScala.map(_.getStringValue)
    }

    def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
      validate()
      require(attributeNamespace.nonEmpty, s"Empty namespace URI not allowed")

      val stream = xdmNode.select(attribute(attributeNamespace, attributeLocalName))
      stream.asOptionalNode.asScala.map(_.getStringValue)
    }

    def attrOption(attributeLocalName: String): Option[String] = {
      validate()

      val stream = xdmNode.select(attribute(attributeLocalName))
      stream.asOptionalNode.asScala.map(_.getStringValue)
    }

    def attr(attributeName: EName): String = {
      validate()

      attrOption(attributeName).get
    }

    def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
      validate()

      attrOption(attributeNamespaceOption, attributeLocalName).get
    }

    def attr(attributeNamespace: String, attributeLocalName: String): String = {
      validate()

      attrOption(attributeNamespace, attributeLocalName).get
    }

    def text: String = {
      validate()

      val stream = xdmNode.select(child(isText))
      stream.asListOfNodes.asScala.to(ArraySeq).map(_.getUnderlyingNode.getStringValue).mkString
    }

    def normalizedText: String = {
      validate()

      // TODO
      ???
    }

    def trimmedText: String = {
      validate()

      text.trim
    }

    // ScopedElemApi

    def scope: Scope = {
      validate()

      val stream = xdmNode.select(child(isNamespace))

      val result = stream.asListOfNodes.asScala.to(ArraySeq).map { n =>
        // Not very transparent: prefix is "display name" and namespace URI is "string value"
        val prefix = n.getUnderlyingNode.getDisplayName
        val nsUri = n.getUnderlyingNode.getStringValue
        (prefix -> nsUri)
      }
      Scope.from(result.to(Map))
    }

    def qname: QName = {
      validate()

      Node.extractQName(xdmNode)
    }

    def attributesByQName: Iterable[(QName, String)] = {
      validate()

      val stream = xdmNode.select(attribute())
      stream.asListOfNodes.asScala.to(ArraySeq).map(n => Node.extractQName(n) -> n.getStringValue)
    }

    def textAsQName: QName = {
      validate()

      QName.parse(text.trim)
    }

    def textAsResolvedQName: EName = {
      validate()

      scope.resolveQNameOption(textAsQName).getOrElse(
        sys.error(s"Could not resolve QName-valued element text $textAsQName, given scope [${scope}]"))
    }

    def attrAsQNameOption(attributeName: EName): Option[QName] = {
      validate()

      attrOption(attributeName).map(v => QName.parse(v.trim))
    }

    def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
      validate()

      attrOption(attributeNamespaceOption, attributeLocalName).map(v => QName.parse(v.trim))
    }

    def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
      validate()

      attrOption(attributeNamespace, attributeLocalName).map(v => QName.parse(v.trim))
    }

    def attrAsQName(attributeName: EName): QName = {
      validate()

      attrAsQNameOption(attributeName).getOrElse(
        sys.error(s"Missing QName-valued attribute $attributeName"))
    }

    def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
      validate()

      attrAsQNameOption(attributeNamespaceOption, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
    }

    def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
      validate()

      attrAsQNameOption(attributeNamespace, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
    }

    def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
      validate()

      attrAsQNameOption(attributeName).map { qn =>
        scope.resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope}]"))
      }
    }

    def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
      validate()

      attrAsQNameOption(attributeNamespaceOption, attributeLocalName).map { qn =>
        scope.resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope}]"))
      }
    }

    def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
      validate()

      attrAsQNameOption(attributeNamespace, attributeLocalName).map { qn =>
        scope.resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope}]"))
      }
    }

    def attrAsResolvedQName(attributeName: EName): EName = {
      validate()

      attrAsResolvedQNameOption(attributeName).getOrElse(
        sys.error(s"Missing QName-valued attribute $attributeName"))
    }

    def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
      validate()

      attrAsResolvedQNameOption(attributeNamespaceOption, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
    }

    def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
      validate()

      attrAsResolvedQNameOption(attributeNamespace, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
    }

    // BackingElemApi

    def findParentElem(p: ThisElem => Boolean): Option[ThisElem] = {
      validate()

      val stream = xdmNode.select(parent(isElement).where(n => p(Elem(n))))
      stream.asOptionalNode.asScala.map(n => Elem(n))
    }

    def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      validate()

      val stream = xdmNode.select(ancestor(isElement).where(n => p(Elem(n))))
      stream.asListOfNodes.asScala.to(ArraySeq).map(n => Elem(n))
    }

    def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = {
      validate()

      val stream = xdmNode.select(ancestor(isElement).where(n => p(Elem(n))))
      stream.asOptionalNode.asScala.map(n => Elem(n))
    }

    def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      validate()

      val stream = xdmNode.select(ancestorOrSelf(isElement).where(n => p(Elem(n))))
      stream.asListOfNodes.asScala.to(ArraySeq).map(n => Elem(n))
    }

    def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      validate()

      val stream = xdmNode.select(ancestorOrSelf(isElement).where(n => p(Elem(n))))
      stream.asOptionalNode.asScala.map(n => Elem(n))
    }

    def baseUriOption: Option[URI] = {
      validate()

      Option(xdmNode.getUnderlyingNode.getBaseURI).map(u => URI.create(u))
    }

    def baseUri: URI = {
      validate()

      baseUriOption.getOrElse(Node.EmptyUri)
    }

    def docUriOption: Option[URI] = {
      validate()

      Option(xdmNode.getUnderlyingNode.getSystemId).map(u => URI.create(u))
    }

    def docUri: URI = {
      validate()

      docUriOption.getOrElse(Node.EmptyUri)
    }

    def rootElem: ThisElem = {
      validate()

      filterAncestorElemsOrSelf(_ => true).last
    }

    // ClarkNodes.Elem

    def children: Seq[ThisNode] = {
      validate()

      val stream = xdmNode.select(child())
      stream.asListOfNodes.asScala.to(ArraySeq).flatMap(n => Node.opt(n))
    }

    // Other methods

    def validate(): Unit = {
      require(xdmNode.getNodeKind == XdmNodeKind.ELEMENT, s"Not an element node: $xdmNode")
    }
  }

  /**
   * Saxon text node
   */
  final case class Text(xdmNode: XdmNode) extends AnyVal with Node with BackingNodes.Text {

    def text: String = {
      validate()
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
      validate()
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
      validate()
      xdmNode.getUnderlyingNode.getDisplayName
    }

    def data: String = {
      validate()
      xdmNode.getUnderlyingNode.getStringValue
    }

    def validate(): Unit = {
      require(xdmNode.getNodeKind == XdmNodeKind.PROCESSING_INSTRUCTION, s"Not a processing instruction node: $xdmNode")
    }
  }

  object Node {

    def opt(xdmNode: XdmNode): Option[Node] = {
      xdmNode.getNodeKind match {
        case XdmNodeKind.ELEMENT => Some(Elem(xdmNode))
        case XdmNodeKind.TEXT => Some(Text(xdmNode))
        case XdmNodeKind.COMMENT => Some(Comment(xdmNode))
        case XdmNodeKind.PROCESSING_INSTRUCTION => Some(ProcessingInstruction(xdmNode))
        case _ => None
      }
    }

    def extractEName(xdmNode: XdmNode): EName = {
      val ns: String = xdmNode.getUnderlyingNode.getURI
      val nsOption: Option[String] = if (ns == "") None else Some(ns)
      EName(nsOption, xdmNode.getUnderlyingNode.getLocalPart)
    }

    def extractQName(xdmNode: XdmNode): QName = {
      val pref: String = xdmNode.getUnderlyingNode.getPrefix
      val prefOption: Option[String] = if (pref == "") None else Some(pref)
      QName(prefOption, xdmNode.getUnderlyingNode.getLocalPart)
    }

    private[saxon] val EmptyUri: URI = URI.create("")
  }
}
