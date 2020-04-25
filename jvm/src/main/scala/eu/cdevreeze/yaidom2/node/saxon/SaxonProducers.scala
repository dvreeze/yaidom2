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

import eu.cdevreeze.yaidom2.creationapi.BackingDocumentFactory
import eu.cdevreeze.yaidom2.creationapi.BackingNodeFactories
import eu.cdevreeze.yaidom2.jaxp.SaxEventProducers
import eu.cdevreeze.yaidom2.queryapi.BackingDocumentApi
import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import net.sf.saxon.s9api.Processor
import org.xml.sax.ContentHandler

/**
 * Saxon document and node producers, from any backing document or node.
 *
 * @author Chris de Vreeze
 */
object SaxonProducers {

  def documentProducer(processor: Processor): DocumentProducer = new DocumentProducer(processor)

  def elementProducer(processor: Processor): ElemProducer = new ElemProducer(processor)

  /**
   * Creates a SaxonDocument from a SaxonNodes.Elem document element. This is an somewhat expensive call.
   */
  def makeDocument(docElem: SaxonNodes.Elem): SaxonDocument = {
    val processor = docElem.xdmNode.getProcessor
    val saxonDocBuilder = processor.newDocumentBuilder()
    docElem.docUriOption.foreach(docUri => saxonDocBuilder.setBaseURI(docUri))
    val buildingContentHandler = saxonDocBuilder.newBuildingContentHandler()

    produceEventsForDocumentFromRootElem(docElem, buildingContentHandler)
    SaxonDocument(buildingContentHandler.getDocumentNode)
  }

  /**
   * SaxonDocument factory from backing documents. The factory constructor takes a Saxon Processor.
   */
  final class DocumentProducer(val processor: Processor) extends BackingDocumentFactory {

    type TargetDocumentType = SaxonDocument

    def from(document: BackingDocumentApi): SaxonDocument = {
      document match {
        case doc: SaxonDocument =>
          doc
        case doc =>
          val saxonDocBuilder = processor.newDocumentBuilder()
          saxonDocBuilder.setBaseURI(doc.docUriOption.getOrElse(new URI("")))
          val buildingContentHandler = saxonDocBuilder.newBuildingContentHandler()

          SaxEventProducers.produceEventsForDocument(doc, buildingContentHandler)
          SaxonDocument(buildingContentHandler.getDocumentNode)
      }
    }
  }

  /**
   * SaxonNodes.Elem factory from backing elements. The factory constructor takes a Saxon Processor.
   * This element producer should be used only for document elements of a document.
   */
  final class ElemProducer(val processor: Processor) extends BackingNodeFactories.ElemFactory {

    type TargetElemType = SaxonNodes.Elem

    /**
     * Creates a SaxonNodes.Elem from the given BackingNodes.Elem.
     * The passed element should be the document element of a document.
     */
    def from(elm: BackingNodes.Elem): SaxonNodes.Elem = {
      elm match {
        case elm: SaxonNodes.Elem =>
          elm
        case elm =>
          val saxonDocBuilder = processor.newDocumentBuilder()
          saxonDocBuilder.setBaseURI(elm.docUri)
          val buildingContentHandler = saxonDocBuilder.newBuildingContentHandler()

          produceEventsForDocumentFromRootElem(elm, buildingContentHandler)
          SaxonDocument(buildingContentHandler.getDocumentNode).documentElement
      }
    }

    /**
     * Creates a SaxonNodes.Elem from the given optional document URI and ScopedNodes.Elem.
     * The passed element should be the document element of a document.
     */
    def from(docUriOption: Option[URI], elm: ScopedNodes.Elem): SaxonNodes.Elem = {
      val saxonDocBuilder = processor.newDocumentBuilder()
      docUriOption.foreach(docUri => saxonDocBuilder.setBaseURI(docUri))
      val buildingContentHandler = saxonDocBuilder.newBuildingContentHandler()

      produceEventsForDocumentFromRootElem(elm, buildingContentHandler)
      SaxonDocument(buildingContentHandler.getDocumentNode).documentElement
    }
  }

  /**
   * Calls method `SaxEventProducers.produceEventsForDocumentFromRootElem`.
   */
  private def produceEventsForDocumentFromRootElem(elem: ScopedNodes.Elem, contentHandler: ContentHandler): Unit = {
    SaxEventProducers.produceEventsForDocumentFromRootElem(elem, contentHandler)
  }

  // No BackingNodeFactories.NodeFactory implementation
}
