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

package eu.cdevreeze.yaidom2.node.resolved

import scala.collection.immutable
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap
import scala.collection.mutable

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.oofun.ClarkElemFunctionWrapper

/**
 * "Resolved" nodes.
 *
 * @author Chris de Vreeze
 */
object ResolvedNodes {

  // First the OO query API

  /**
   * Arbitrary resolved node
   */
  sealed trait Node extends ClarkNodes.Node

  /**
   * Potential document child (although there are no "resolved documents")
   */
  sealed trait CanBeDocumentChild extends Node with ClarkNodes.CanBeDocumentChild

  /**
   * "Resolved" element node, offering the `ClarkNodes.Elem` element query API.
   */
  final case class Elem(
    name: EName,
    attributes: SeqMap[EName, String],
    children: Seq[Node]
  ) extends CanBeDocumentChild with ClarkNodes.Elem {

    type ThisElem = Elem

    type ThisNode = Node

    // ElemApi

    def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
      children.collect { case e@Elem(_, _, _) if p(e) => e }
    }

    def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
      children.collectFirst { case e@Elem(_, _, _) if p(e) => e }
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
      children.collect { case Text(t) => t }.mkString
    }

    def normalizedText: String = {
      normalizeString(text)
    }

    def trimmedText: String = {
      text.trim
    }

    // ClarkNodes.Elem

    def select(step: ElemStep[Elem]): Seq[Elem] = {
      step(this)
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
   * "Resolved" text node
   */
  final case class Text(text: String) extends Node with ClarkNodes.Text

  // Next the functional query (and creation) API

  object Node extends ElemCreationApi {

    type NodeType = Node

    type ElemType = Elem

    def from(node: ClarkNodes.Node): Node = node match {
      case e: ClarkNodes.Elem => Elem.from(e)
      case t: ClarkNodes.Text => Text(t.text)
      case n => sys.error(s"Not an element or text node: $n")
    }

    def elem(name: EName, children: Seq[NodeType]): ElemType = {
      Elem(name, SeqMap.empty, children)
    }

    def elem(name: EName, attributes: immutable.Iterable[(EName, String)], children: Seq[NodeType]): ElemType = {
      Elem(name, attributes.to(SeqMap), children)
    }

    def textElem(name: EName, txt: String): ElemType = {
      Elem(name, SeqMap.empty, ArraySeq(Text(txt)))
    }

    def textElem(name: EName, attributes: immutable.Iterable[(EName, String)], txt: String): ElemType = {
      Elem(name, attributes.to(SeqMap), ArraySeq(Text(txt)))
    }

    def emptyElem(name: EName): ElemType = {
      Elem(name, SeqMap.empty, ArraySeq.empty)
    }

    def emptyElem(name: EName, attributes: immutable.Iterable[(EName, String)]): ElemType = {
      Elem(name, attributes.to(SeqMap), ArraySeq.empty)
    }
  }

  object Elem extends ClarkElemFunctionWrapper {

    type ElemType = Elem

    type NodeType = Node

    def from(elm: ClarkNodes.Elem): Elem = {
      val children = elm.children.collect {
        case childElm: ClarkNodes.Elem => childElm
        case childText: ClarkNodes.Text => childText
      }
      // Recursion, with Node.from and Elem.from being mutually dependent
      val resolvedChildren = children.map { node => Node.from(node) }

      Elem(elm.name, elm.attributes, resolvedChildren)
    }
  }

}
