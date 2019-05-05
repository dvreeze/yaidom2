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

import eu.cdevreeze.yaidom2.creationapi.BackingDocumentConverter
import eu.cdevreeze.yaidom2.jaxp.SaxEventProducers
import eu.cdevreeze.yaidom2.queryapi.oo.BackingDocumentApi
import net.sf.saxon.event.ReceivingContentHandler
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.tree.tiny.TinyBuilder

/**
 * Saxon document and node producers, from any backing document or node.
 *
 * @author Chris de Vreeze
 */
object SaxonProducers {

  /**
   * SaxonDocument factory from backing documents. The factory constructor takes a Saxon Processor, which must
   * use the tiny tree model under the hood.
   */
  final class DocumentProducer(val processor: Processor) extends BackingDocumentConverter {

    type TargetDocumentType = SaxonDocument

    def from(document: BackingDocumentApi): SaxonDocument = {
      // See http://saxon-xslt-and-xquery-processor.13853.n7.nabble.com/Constructing-a-tiny-tree-from-SAX-events-td5192.html.
      // The idea is that yaidom2 can convert a backing document to SAX events pushed on any SAX handler, and that the
      // SAX handler used here is a Saxon ReceivingContentHandler, which uses a Saxon TinyBuilder as Saxon Receiver.

      val pipe = processor.getUnderlyingConfiguration.makePipelineConfiguration()

      val builder = new TinyBuilder(pipe)

      if (document.docUriOption.isDefined) {
        builder.setSystemId(document.docUriOption.get.toString)
      }

      val receivingContentHandler = new ReceivingContentHandler()
      receivingContentHandler.setPipelineConfiguration(pipe)
      receivingContentHandler.setReceiver(builder)

      SaxEventProducers.produceEventsForDocument(document, receivingContentHandler)

      val nodeInfo = builder.getCurrentRoot

      if (document.docUriOption.isDefined) {
        require(
          nodeInfo.getSystemId == document.docUriOption.get.toString,
          s"Expected document URI '${document.docUriOption.get}' but encountered document URI '${nodeInfo.getSystemId}'")
      }

      val saxonDoc = SaxonDocument(new XdmNode(nodeInfo))
      saxonDoc
    }
  }
}
