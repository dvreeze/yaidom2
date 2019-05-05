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

package eu.cdevreeze.yaidom2.jaxp

import eu.cdevreeze.yaidom2.core.Declarations
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.oo.ScopedDocumentApi
import eu.cdevreeze.yaidom2.queryapi.oo.ScopedNodes
import org.xml.sax.Attributes
import org.xml.sax.ContentHandler
import org.xml.sax.ext.LexicalHandler
import org.xml.sax.helpers.AttributesImpl

/**
 * SAX event producers for Scoped nodes and documents.
 *
 * @author Chris de Vreeze
 */
object SaxEventProducers {

  def produceEventsForDocument(doc: ScopedDocumentApi, contentHandler: ContentHandler): Unit = {
    contentHandler.startDocument()
    doc.children.foreach { ch => produceEventsForNode(ch, Scope.Empty, contentHandler) }
    contentHandler.endDocument()
  }

  def produceEventsForElem(elem: ScopedNodes.Elem, parentScope: Scope, contentHandler: ContentHandler): Unit = {
    val namespaces: Declarations = parentScope.relativize(elem.scope)
    val namespacesMap = namespaces.prefixNamespaceMap

    for ((prefix, nsUri) <- namespacesMap) contentHandler.startPrefixMapping(prefix, nsUri)

    generateStartElementEvent(elem, parentScope, contentHandler)

    // Recursive calls. Not tail-recursive, but recursion depth should be limited.

    for (node <- elem.children) {
      produceEventsForNode(node, elem.scope, contentHandler)
    }

    generateEndElementEvent(elem, parentScope, contentHandler)

    for ((prefix, nsUri) <- namespacesMap) contentHandler.endPrefixMapping(prefix)
  }

  def produceEventsForNode(node: ScopedNodes.Node, parentScope: Scope, contentHandler: ContentHandler): Unit = {
    node match {
      case e: ScopedNodes.Elem =>
        produceEventsForElem(e, parentScope, contentHandler)
      case t: ScopedNodes.Text =>
        produceEventsForText(t, contentHandler)
      case c: ScopedNodes.Comment =>
        produceEventsForComment(c, contentHandler)
      case pi: ScopedNodes.ProcessingInstruction =>
        produceEventsForProcessingInstruction(pi, contentHandler)
    }
  }

  private def produceEventsForText(text: ScopedNodes.Text, contentHandler: ContentHandler): Unit = {
    // TODO Add isCData to ScopedNodes.Text, without it affecting equality
    contentHandler.characters(text.text.toCharArray, 0, text.text.length)
  }

  private def produceEventsForComment(comment: ScopedNodes.Comment, contentHandler: ContentHandler): Unit = {
    contentHandler match {
      case handler: ContentHandler with LexicalHandler =>
        handler.comment(comment.text.toCharArray, 0, comment.text.length)
      case _ => ()
    }
  }

  private def produceEventsForProcessingInstruction(
    processingInstruction: ScopedNodes.ProcessingInstruction,
    contentHandler: ContentHandler): Unit = {

    contentHandler.processingInstruction(processingInstruction.target, processingInstruction.data)
  }

  private def generateStartElementEvent(elem: ScopedNodes.Elem, parentScope: Scope, handler: ContentHandler): Unit = {
    val uri = elem.namespaceAsString

    val attrs: Attributes = getAttributes(elem)

    handler.startElement(uri, elem.localName, elem.qname.toString, attrs)
  }

  private def generateEndElementEvent(elem: ScopedNodes.Elem, parentScope: Scope, handler: ContentHandler): Unit = {
    val uri = elem.namespaceAsString

    handler.endElement(uri, elem.localName, elem.qname.toString)
  }

  private def getAttributes(elem: ScopedNodes.Elem): Attributes = {
    val attrs = new AttributesImpl

    addNormalAttributes(elem, attrs)
    attrs
  }

  /**
   * Gets the normal (non-namespace-declaration) attributes, and adds them to the passed Attributes object.
   * This method is called internally, providing the attributes that are passed to the startElement call.
   */
  private def addNormalAttributes(elem: ScopedNodes.Elem, attrs: AttributesImpl): Attributes = {
    val attrScope = elem.scope.withoutDefaultNamespace

    for ((attrQName, attrValue) <- elem.attributesByQName) {
      val attrEName = attrScope.resolveQNameOption(attrQName).getOrElse(sys.error(s"Corrupt non-resolvable attribute: $attrQName"))
      val uri = attrEName.namespaceUriOption.getOrElse("")
      val tpe = "CDATA"

      attrs.addAttribute(uri, attrQName.localPart, attrQName.toString, tpe, attrValue)
    }

    attrs
  }
}
