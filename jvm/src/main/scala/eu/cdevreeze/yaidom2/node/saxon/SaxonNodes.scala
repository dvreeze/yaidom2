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

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap
import scala.jdk.OptionConverters._
import scala.jdk.StreamConverters._

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.XdmNodeKind
import net.sf.saxon.s9api.streams.Predicates._
import net.sf.saxon.s9api.streams.Step
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
  sealed trait Node extends BackingNodes.Node {

    def xdmNode: XdmNode
  }

  /**
   * Potential Saxon document child, so an element, processing instruction or comment
   */
  sealed trait CanBeDocumentChild extends Node with BackingNodes.CanBeDocumentChild

  /**
   * Saxon element node, offering the `BackingNodes.Elem` element query API.
   *
   * Creation of this element is cheap, because the only state is the underlying Saxon XdmNode.
   */
  // scalastyle:off number.of.methods
  final case class Elem(xdmNode: XdmNode) extends CanBeDocumentChild with BackingNodes.Elem {
    require(xdmNode.getNodeKind == XdmNodeKind.ELEMENT, s"Not an element node: $xdmNode")

    type ThisElem = Elem

    type ThisNode = Node

    // ElemApi

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterChildElems(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllChildElems: Seq[ThisElem] = {
      Elem.findAllChildElems(xdmNode).map(n => Elem(n))
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findChildElem(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterDescendantElems(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllDescendantElems: Seq[ThisElem] = {
      Elem.findAllDescendantElems(xdmNode).map(n => Elem(n))
    }

    def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findDescendantElem(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterDescendantElemsOrSelf(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllDescendantElemsOrSelf: Seq[ThisElem] = {
      Elem.findAllDescendantElemsOrSelf(xdmNode).map(n => Elem(n))
    }

    def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findDescendantElemOrSelf(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.findTopmostElems(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.findTopmostElemsOrSelf(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findDescendantElemOrSelf(navigationPath: Seq[Int]): Option[ThisElem] = {
      Elem.findDescendantElemOrSelf(xdmNode, navigationPath).map(n => Elem(n))
    }

    def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = {
      Elem(Elem.getDescendantElemOrSelf(xdmNode, navigationPath))
    }

    // ClarkElemApi

    def name: EName = {
      Elem.name(xdmNode)
    }

    def attributes: SeqMap[EName, String] = {
      Elem.attributes(xdmNode)
    }

    def localName: String = {
      Elem.localName(xdmNode)
    }

    def namespaceOption: Option[String] = {
      Elem.namespaceOption(xdmNode)
    }

    def namespaceAsString: String = {
      Elem.namespaceAsString(xdmNode)
    }

    def attrOption(attributeName: EName): Option[String] = {
      Elem.attrOption(xdmNode, attributeName)
    }

    def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
      Elem.attrOption(xdmNode, attributeNamespaceOption, attributeLocalName)
    }

    def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
      Elem.attrOption(xdmNode, attributeNamespace, attributeLocalName)
    }

    def attrOption(attributeLocalName: String): Option[String] = {
      Elem.attrOption(xdmNode, attributeLocalName)
    }

    def attr(attributeName: EName): String = {
      Elem.attr(xdmNode, attributeName)
    }

    def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
      Elem.attr(xdmNode, attributeNamespaceOption, attributeLocalName)
    }

    def attr(attributeNamespace: String, attributeLocalName: String): String = {
      Elem.attr(xdmNode, attributeNamespace, attributeLocalName)
    }

    def attr(attributeLocalName: String): String = {
      Elem.attr(xdmNode, attributeLocalName)
    }

    def text: String = {
      Elem.text(xdmNode)
    }

    def normalizedText: String = {
      Elem.normalizedText(xdmNode)
    }

    def trimmedText: String = {
      Elem.trimmedText(xdmNode)
    }

    // ScopedElemApi

    def scope: Scope = {
      Elem.scope(xdmNode)
    }

    def qname: QName = {
      Elem.qname(xdmNode)
    }

    def attributesByQName: SeqMap[QName, String] = {
      Elem.attributesByQName(xdmNode)
    }

    def textAsQName: QName = {
      Elem.textAsQName(xdmNode)
    }

    def textAsResolvedQName: EName = {
      Elem.textAsResolvedQName(xdmNode)
    }

    def attrAsQNameOption(attributeName: EName): Option[QName] = {
      Elem.attrAsQNameOption(xdmNode, attributeName)
    }

    def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
      Elem.attrAsQNameOption(xdmNode, attributeNamespaceOption, attributeLocalName)
    }

    def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
      Elem.attrAsQNameOption(xdmNode, attributeNamespace, attributeLocalName)
    }

    def attrAsQName(attributeName: EName): QName = {
      Elem.attrAsQName(xdmNode, attributeName)
    }

    def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
      Elem.attrAsQName(xdmNode, attributeNamespaceOption, attributeLocalName)
    }

    def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
      Elem.attrAsQName(xdmNode, attributeNamespace, attributeLocalName)
    }

    def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
      Elem.attrAsResolvedQNameOption(xdmNode, attributeName)
    }

    def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
      Elem.attrAsResolvedQNameOption(xdmNode, attributeNamespaceOption, attributeLocalName)
    }

    def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
      Elem.attrAsResolvedQNameOption(xdmNode, attributeNamespace, attributeLocalName)
    }

    def attrAsResolvedQName(attributeName: EName): EName = {
      Elem.attrAsResolvedQName(xdmNode, attributeName)
    }

    def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
      Elem.attrAsResolvedQName(xdmNode, attributeNamespaceOption, attributeLocalName)
    }

    def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
      Elem.attrAsResolvedQName(xdmNode, attributeNamespace, attributeLocalName)
    }

    // BackingElemApi

    def findParentElem(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findParentElem(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findParentElem: Option[ThisElem] = {
      Elem.findParentElem(xdmNode).map(n => Elem(n))
    }

    def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterAncestorElems(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllAncestorElems: Seq[ThisElem] = {
      Elem.findAllAncestorElems(xdmNode).map(n => Elem(n))
    }

    def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findAncestorElem(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
      Elem.filterAncestorElemsOrSelf(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllAncestorElemsOrSelf: Seq[ThisElem] = {
      Elem.findAllAncestorElemsOrSelf(xdmNode).map(n => Elem(n))
    }

    def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
      Elem.findAncestorElemOrSelf(xdmNode, n => p(Elem(n))).map(n => Elem(n))
    }

    def findAllPrecedingSiblingElems: Seq[ThisElem] = {
      Elem.findAllPrecedingSiblingElems(xdmNode).map(n => Elem(n))
    }

    def ownNavigationPathRelativeToRootElem: Seq[Int] = {
      Elem.ownNavigationPathRelativeToRootElem(xdmNode)
    }

    def baseUriOption: Option[URI] = {
      Elem.baseUriOption(xdmNode)
    }

    def baseUri: URI = {
      Elem.baseUri(xdmNode)
    }

    def docUriOption: Option[URI] = {
      Elem.docUriOption(xdmNode)
    }

    def docUri: URI = {
      Elem.docUri(xdmNode)
    }

    def rootElem: ThisElem = {
      Elem(Elem.rootElem(xdmNode))
    }

    // ClarkNodes.Elem

    def children: Seq[ThisNode] = {
      Elem.children(xdmNode).flatMap(n => Node.opt(n))
    }

    def select(step: ElemStep[Elem]): Seq[Elem] = {
      // Implemented directly, instead of in terms of Elem.select.
      step(this)
    }
  }

  /**
   * Saxon text node
   */
  final case class Text(xdmNode: XdmNode) extends Node with BackingNodes.Text {
    require(xdmNode.getNodeKind == XdmNodeKind.TEXT, s"Not a text node: $xdmNode")

    def text: String = {
      xdmNode.getUnderlyingNode.getStringValue
    }
  }

  /**
   * Saxon comment node
   */
  final case class Comment(xdmNode: XdmNode) extends CanBeDocumentChild with BackingNodes.Comment {
    require(xdmNode.getNodeKind == XdmNodeKind.COMMENT, s"Not a comment node: $xdmNode")

    def text: String = {
      xdmNode.getUnderlyingNode.getStringValue
    }
  }

  /**
   * Saxon processing instruction node
   */
  final case class ProcessingInstruction(xdmNode: XdmNode) extends CanBeDocumentChild with BackingNodes.ProcessingInstruction {
    require(xdmNode.getNodeKind == XdmNodeKind.PROCESSING_INSTRUCTION, s"Not a processing instruction node: $xdmNode")

    def target: String = {
      xdmNode.getUnderlyingNode.getDisplayName
    }

    def data: String = {
      xdmNode.getUnderlyingNode.getStringValue
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

  object CanBeDocumentChild {

    def opt(xdmNode: XdmNode): Option[CanBeDocumentChild] = {
      xdmNode.getNodeKind match {
        case XdmNodeKind.ELEMENT => Some(Elem(xdmNode))
        case XdmNodeKind.COMMENT => Some(Comment(xdmNode))
        case XdmNodeKind.PROCESSING_INSTRUCTION => Some(ProcessingInstruction(xdmNode))
        case _ => None
      }
    }
  }

  // scalastyle:off number.of.methods
  private[saxon] object Elem {

    type ElemType = XdmNode

    type NodeType = XdmNode

    def filterChildElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      filterElems(elem, child(), p)
    }

    def findAllChildElems(elem: ElemType): Seq[ElemType] = {
      filterElems(elem, child(), _ => true)
    }

    def findChildElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      findElem(elem, child(), p)
    }

    def filterDescendantElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      filterElems(elem, descendant(), p)
    }

    def findAllDescendantElems(elem: ElemType): Seq[ElemType] = {
      filterElems(elem, descendant(), _ => true)
    }

    def findDescendantElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      findElem(elem, descendant(), p)
    }

    def filterDescendantElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      filterElems(elem, descendantOrSelf(), p)
    }

    def findAllDescendantElemsOrSelf(elem: ElemType): Seq[ElemType] = {
      filterElems(elem, descendantOrSelf(), _ => true)
    }

    def findDescendantElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      findElem(elem, descendantOrSelf(), p)
    }

    def findTopmostElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      val childElemStream = elem.select(child().where(n => isElement.test(n)))

      childElemStream.flatMap(e => findTopmostElemsOrSelfAsStream(e, p)).toScala(ArraySeq)
    }

    def findTopmostElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      findTopmostElemsOrSelfAsStream(elem, p).toScala(ArraySeq)
    }

    def findDescendantElemOrSelf(elem: ElemType, navigationPath: Seq[Int]): Option[ElemType] = {
      if (navigationPath.isEmpty) {
        Some(elem)
      } else {
        val childElemIdx: Int = navigationPath.head
        val childElems: Seq[ElemType] = findAllChildElems(elem)

        if (childElemIdx >= 0 && childElemIdx < childElems.size) {
          // Recursive call
          Option(childElems(childElemIdx)).flatMap(che => findDescendantElemOrSelf(che, navigationPath.drop(1)))
        } else {
          None
        }
      }
    }

    def getDescendantElemOrSelf(elem: ElemType, navigationPath: Seq[Int]): ElemType = {
      findDescendantElemOrSelf(elem, navigationPath).getOrElse(sys.error(s"Missing element at navigation path $navigationPath"))
    }

    def name(elem: ElemType): EName = {
      Node.extractEName(elem)
    }

    def attributes(elem: ElemType): SeqMap[EName, String] = {
      val stream = elem.select(attribute())
      stream.toScala(ArraySeq).map(n => Node.extractEName(n) -> n.getStringValue).to(SeqMap)
    }

    def localName(elem: ElemType): String = {
      elem.getUnderlyingNode.getLocalPart
    }

    def namespaceOption(elem: ElemType): Option[String] = {
      val nsAsString = namespaceAsString(elem)

      if (nsAsString.isEmpty) None else Some(nsAsString)
    }

    def namespaceAsString(elem: ElemType): String = {
      elem.getUnderlyingNode.getURI
    }

    def attrOption(elem: ElemType, attributeName: EName): Option[String] = {
      val stream = elem.select(attribute(attributeName.namespaceUriOption.getOrElse(""), attributeName.localPart))
      stream.asOptionalNode.toScala.map(_.getStringValue)
    }

    def attrOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
      val stream = elem.select(attribute(attributeNamespaceOption.getOrElse(""), attributeLocalName))
      stream.asOptionalNode.toScala.map(_.getStringValue)
    }

    def attrOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[String] = {
      require(attributeNamespace.nonEmpty, s"Empty namespace URI not allowed")

      val stream = elem.select(attribute(attributeNamespace, attributeLocalName))
      stream.asOptionalNode.toScala.map(_.getStringValue)
    }

    def attrOption(elem: ElemType, attributeLocalName: String): Option[String] = {
      val stream = elem.select(attribute("", attributeLocalName))
      stream.asOptionalNode.toScala.map(_.getStringValue)
    }

    def attr(elem: ElemType, attributeName: EName): String = {
      attrOption(elem, attributeName).get
    }

    def attr(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
      attrOption(elem, attributeNamespaceOption, attributeLocalName).get
    }

    def attr(elem: ElemType, attributeNamespace: String, attributeLocalName: String): String = {
      attrOption(elem, attributeNamespace, attributeLocalName).get
    }

    def attr(elem: ElemType, attributeLocalName: String): String = {
      attrOption(elem, attributeLocalName).get
    }

    def text(elem: ElemType): String = {
      val stream = elem.select(child(isText))
      stream.toScala(ArraySeq).map(_.getUnderlyingNode.getStringValue).mkString
    }

    def normalizedText(elem: ElemType): String = {
      normalizeString(text(elem))
    }

    def trimmedText(elem: ElemType): String = {
      text(elem).trim
    }

    def children(elem: ElemType): Seq[NodeType] = {
      val stream = elem.select(child())
      stream.toScala(ArraySeq)
    }

    def select(elem: ElemType, step: ElemStep[ElemType]): Seq[ElemType] = {
      step(elem)
    }

    def scope(elem: ElemType): Scope = {
      val stream = elem.select(namespace())

      val result = stream.toScala(ArraySeq).map { n =>
        // Not very transparent: prefix is "display name" and namespace URI is "string value"
        val prefix = n.getUnderlyingNode.getDisplayName
        val nsUri = n.getUnderlyingNode.getStringValue
        prefix -> nsUri
      }
      Scope.from(result.to(SeqMap))
    }

    def qname(elem: ElemType): QName = {
      Node.extractQName(elem)
    }

    def attributesByQName(elem: ElemType): SeqMap[QName, String] = {
      val stream = elem.select(attribute())
      stream.toScala(ArraySeq).map(n => Node.extractQName(n) -> n.getStringValue).to(SeqMap)
    }

    def textAsQName(elem: ElemType): QName = {
      QName.parse(text(elem).trim)
    }

    def textAsResolvedQName(elem: ElemType): EName = {
      scope(elem).resolveQNameOption(textAsQName(elem)).getOrElse(
        sys.error(s"Could not resolve QName-valued element text ${textAsQName(elem)}, given scope [${scope(elem)}]"))
    }

    def attrAsQNameOption(elem: ElemType, attributeName: EName): Option[QName] = {
      attrOption(elem, attributeName).map(v => QName.parse(v.trim))
    }

    def attrAsQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
      attrOption(elem, attributeNamespaceOption, attributeLocalName).map(v => QName.parse(v.trim))
    }

    def attrAsQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[QName] = {
      attrOption(elem, attributeNamespace, attributeLocalName).map(v => QName.parse(v.trim))
    }

    def attrAsQName(elem: ElemType, attributeName: EName): QName = {
      attrAsQNameOption(elem, attributeName).getOrElse(
        sys.error(s"Missing QName-valued attribute $attributeName"))
    }

    def attrAsQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
      attrAsQNameOption(elem, attributeNamespaceOption, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
    }

    def attrAsQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): QName = {
      attrAsQNameOption(elem, attributeNamespace, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
    }

    def attrAsResolvedQNameOption(elem: ElemType, attributeName: EName): Option[EName] = {
      attrAsQNameOption(elem, attributeName).map { qn =>
        scope(elem).resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope(elem)}]"))
      }
    }

    def attrAsResolvedQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
      attrAsQNameOption(elem, attributeNamespaceOption, attributeLocalName).map { qn =>
        scope(elem).resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope(elem)}]"))
      }
    }

    def attrAsResolvedQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[EName] = {
      attrAsQNameOption(elem, attributeNamespace, attributeLocalName).map { qn =>
        scope(elem).resolveQNameOption(qn).getOrElse(
          sys.error(s"Could not resolve QName-valued attribute value $qn, given scope [${scope(elem)}]"))
      }
    }

    def attrAsResolvedQName(elem: ElemType, attributeName: EName): EName = {
      attrAsResolvedQNameOption(elem, attributeName).getOrElse(
        sys.error(s"Missing QName-valued attribute $attributeName"))
    }

    def attrAsResolvedQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
      attrAsResolvedQNameOption(elem, attributeNamespaceOption, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(attributeNamespaceOption, attributeLocalName)}"))
    }

    def attrAsResolvedQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): EName = {
      attrAsResolvedQNameOption(elem, attributeNamespace, attributeLocalName).getOrElse(
        sys.error(s"Missing QName-valued attribute ${EName(Some(attributeNamespace), attributeLocalName)}"))
    }

    def findParentElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      findElem(elem, parent(), p)
    }

    def findParentElem(elem: ElemType): Option[ElemType] = {
      findElem(elem, parent(), _ => true)
    }

    def filterAncestorElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      filterElems(elem, ancestor(), p)
    }

    def findAllAncestorElems(elem: ElemType): Seq[ElemType] = {
      filterElems(elem, ancestor(), _ => true)
    }

    def findAncestorElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      findElem(elem, ancestor(), p)
    }

    def filterAncestorElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
      filterElems(elem, ancestorOrSelf(), p)
    }

    def findAllAncestorElemsOrSelf(elem: ElemType): Seq[ElemType] = {
      filterElems(elem, ancestorOrSelf(), _ => true)
    }

    def findAncestorElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
      findElem(elem, ancestorOrSelf(), p)
    }

    def findAllPrecedingSiblingElems(elem: ElemType): Seq[ElemType] = {
      filterElems(elem, precedingSibling(), _ => true)
    }

    def ownNavigationPathRelativeToRootElem(elem: ElemType): Seq[Int] = {
      def relativeNavigationPath(e: ElemType): Seq[Int] = {
        findParentElem(e).map { pe =>
          // Recursive call
          relativeNavigationPath(pe).appended(findAllPrecedingSiblingElems(e).size)
        }.getOrElse(IndexedSeq.empty)
      }

      relativeNavigationPath(elem).to(ArraySeq)
    }

    def baseUriOption(elem: ElemType): Option[URI] = {
      Option(elem.getUnderlyingNode.getBaseURI).map(u => URI.create(u))
    }

    def baseUri(elem: ElemType): URI = {
      baseUriOption(elem).getOrElse(Node.EmptyUri)
    }

    def docUriOption(elem: ElemType): Option[URI] = {
      Option(elem.getUnderlyingNode.getSystemId).map(u => URI.create(u))
    }

    def docUri(elem: ElemType): URI = {
      docUriOption(elem).getOrElse(Node.EmptyUri)
    }

    def rootElem(elem: ElemType): ElemType = {
      findAllAncestorElemsOrSelf(elem).last
    }

    // Private methods

    private def filterElems(elem: ElemType, step: Step[XdmNode], p: ElemType => Boolean): Seq[ElemType] = {
      val stream = elem.select(step.where(n => isElement.test(n) && p(n)))
      stream.toScala(ArraySeq)
    }

    private def findElem(elem: ElemType, step: Step[XdmNode], p: ElemType => Boolean): Option[ElemType] = {
      val stream = elem.select(step.where(n => isElement.test(n) && p(n)))
      stream.findFirst.toScala
    }

    private def findTopmostElemsOrSelfAsStream(elem: ElemType, p: ElemType => Boolean): java.util.stream.Stream[ElemType] = {
      if (p(elem)) {
        java.util.stream.Stream.of(elem)
      } else {
        val childElemStream = elem.select(child().where(n => isElement.test(n)))

        // Recursive calls
        childElemStream.flatMap(che => findTopmostElemsOrSelfAsStream(che, p))
      }
    }

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

}
