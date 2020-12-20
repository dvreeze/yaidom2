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

package eu.cdevreeze.yaidom2.node.tests

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.net.URI

import eu.cdevreeze.yaidom2.core._
import eu.cdevreeze.yaidom2.creationapi.ScopedDocumentFactory
import eu.cdevreeze.yaidom2.jaxp.DocumentSerializer
import eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilderCreator
import eu.cdevreeze.yaidom2.node.nodebuilder
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.saxon.SaxonDocument
import eu.cdevreeze.yaidom2.node.saxon.SaxonProducers
import eu.cdevreeze.yaidom2.node.saxon.SaxonSerializer
import eu.cdevreeze.yaidom2.queryapi._
import javax.xml.transform.TransformerFactory
import javax.xml.transform.stream.StreamResult
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Codec

abstract class ScopedDocumentRoundtrippingTest[E <: ScopedNodes.Elem.Aux[_, E], D <: ScopedDocumentApi.Aux[_, _, E, D]]
    extends AnyFunSuite {

  protected def doc: D

  protected val documentFactory: ScopedDocumentFactory.Aux[D]

  protected val processor: Processor = new Processor(false)

  test("testRoundtripping") {
    assertResult(true) {
      doc.documentElement.findAllDescendantElemsOrSelf.size >= 10
    }

    val docSerializer = DocumentSerializer(TransformerFactory.newInstance())

    val bos = new ByteArrayOutputStream()
    docSerializer.serializeDocument(doc, new StreamResult(bos), Codec.UTF8.toString)
    val xmlString = new String(bos.toByteArray, Codec.UTF8.toString)

    val parsedDoc: SaxonDocument =
      SaxonProducers
        .parser(processor)
        .parse(new ByteArrayInputStream(xmlString.getBytes(Codec.UTF8.toString)), doc.docUriOption.getOrElse(new URI("")))

    assertResult(resolved.Elem.from(doc.documentElement)) {
      resolved.Elem.from(parsedDoc.documentElement)
    }

    val convertedDoc: D = documentFactory.from(parsedDoc)

    assertResult(resolved.Elem.from(doc.documentElement)) {
      resolved.Elem.from(convertedDoc.documentElement)
    }
  }

  test("testRoundtrippingViaSaxon") {
    assertResult(true) {
      doc.documentElement.findAllDescendantElemsOrSelf.size >= 10
    }

    val saxonDoc: saxon.Document =
      SaxonProducers.makeDocument(SaxonProducers.elementProducer(processor).from(doc.docUriOption, doc.documentElement))

    val bos = new ByteArrayOutputStream()
    SaxonSerializer.serialize(saxonDoc, bos)
    val xmlString = new String(bos.toByteArray, Codec.UTF8.toString)

    val parsedDoc: SaxonDocument =
      SaxonProducers
        .parser(processor)
        .parse(new ByteArrayInputStream(xmlString.getBytes(Codec.UTF8.toString)), doc.docUriOption.getOrElse(new URI("")))

    assertResult(resolved.Elem.from(doc.documentElement)) {
      resolved.Elem.from(parsedDoc.documentElement)
    }

    val convertedDoc: D = documentFactory.from(parsedDoc)

    assertResult(resolved.Elem.from(doc.documentElement)) {
      resolved.Elem.from(convertedDoc.documentElement)
    }
  }

  private val LinkNs = "http://www.xbrl.org/2003/linkbase"
  private val RefNs = "http://www.xbrl.org/2006/ref"
  private val XLinkNs = "http://www.w3.org/1999/xlink"
  private val XsiNs = "http://www.w3.org/2001/XMLSchema-instance"

  protected val nodeBuilderDoc: nodebuilder.Document = {
    val scope = StableScope.from("link" -> LinkNs, "ref" -> RefNs, "xlink" -> XLinkNs, "xsi" -> XsiNs)
    val elemCreator = NodeBuilderCreator(scope)

    import elemCreator._

    val elm: nodebuilder.Elem = emptyElem(q"link:namespace")
      .plusAttribute(q"xsi:schemaLocation", "http://www.xbrl.org/2006/ref http://www.xbrl.org/2006/ref-2006-02-27.xsd")
      .plusChildElem {
        emptyElem(q"link:referenceLink")
          .plusAttribute(q"xlink:role", "http://www.xbrl.org/2003/role/link")
          .plusAttribute(q"xlink:type", "extended")
          .plusChildElem {
            emptyElem(q"link:loc")
              .plusAttribute(q"xlink:href", "jenv-bw2-axes.xsd#jenv-bw2-dim_LiabilitiesOtherAxis")
              .plusAttribute(q"xlink:label", "jenv-bw2-dim_LiabilitiesOtherAxis_loc")
              .plusAttribute(q"xlink:type", "locator")
          }
          .plusChildElem {
            emptyElem(q"link:loc")
              .plusAttribute(q"xlink:href", "jenv-bw2-axes.xsd#jenv-bw2-dim_LoansAdvancesGuaranteesAxis")
              .plusAttribute(q"xlink:label", "jenv-bw2-dim_LoansAdvancesGuaranteesAxis_loc")
              .plusAttribute(q"xlink:type", "locator")
          }
          .plusChildElem {
            emptyElem(q"link:loc")
              .plusAttribute(q"xlink:href", "jenv-bw2-axes.xsd#jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis")
              .plusAttribute(q"xlink:label", "jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis_loc")
              .plusAttribute(q"xlink:type", "locator")
          }
          .plusChildElem {
            emptyElem(q"link:reference")
              .plusAttribute(q"id", "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
              .plusAttribute(q"xlink:label", "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
              .plusAttribute(q"xlink:role", "http://www.xbrl.org/2003/role/reference")
              .plusAttribute(q"xlink:type", "resource")
              .plusChildElem(textElem(q"ref:Article", "383e"))
              .plusChildElem(textElem(q"ref:IssueDate", "2019-01-01"))
              .plusChildElem(textElem(q"ref:Name", "Burgerlijk wetboek boek 2"))
          }
          .plusChildElem {
            emptyElem(q"link:reference")
              .plusAttribute(q"id", "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
              .plusAttribute(q"xlink:label", "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
              .plusAttribute(q"xlink:role", "http://www.xbrl.org/2003/role/reference")
              .plusAttribute(q"xlink:type", "resource")
              .plusChildElem(textElem(q"ref:Chapter", "115"))
              .plusChildElem(textElem(q"ref:IssueDate", "2019-01-01"))
              .plusChildElem(textElem(q"ref:Name", "Richtlijnen voor de jaarverslaggeving"))
              .plusChildElem(textElem(q"ref:Paragraph", "214"))
          }
          .plusChildElem {
            emptyElem(q"link:reference")
              .plusAttribute(q"id", "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
              .plusAttribute(q"xlink:label", "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
              .plusAttribute(q"xlink:role", "http://www.xbrl.org/2003/role/reference")
              .plusAttribute(q"xlink:type", "resource")
              .plusChildElem(textElem(q"ref:Chapter", "610"))
              .plusChildElem(textElem(q"ref:IssueDate", "2019-01-01"))
              .plusChildElem(textElem(q"ref:Name", "Richtlijnen voor de jaarverslaggeving"))
              .plusChildElem(textElem(q"ref:Paragraph", "106"))
          }
          .plusChildElem {
            emptyElem(q"link:referenceArc")
              .plusAttribute(q"xlink:arcrole", "http://www.xbrl.org/2003/arcrole/concept-reference")
              .plusAttribute(q"xlink:from", "jenv-bw2-dim_LiabilitiesOtherAxis_loc")
              .plusAttribute(q"xlink:to", "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
              .plusAttribute(q"xlink:type", "arc")
          }
          .plusChildElem {
            emptyElem(q"link:referenceArc")
              .plusAttribute(q"xlink:arcrole", "http://www.xbrl.org/2003/arcrole/concept-reference")
              .plusAttribute(q"xlink:from", "jenv-bw2-dim_LoansAdvancesGuaranteesAxis_loc")
              .plusAttribute(q"xlink:to", "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
              .plusAttribute(q"xlink:type", "arc")
          }
          .plusChildElem {
            emptyElem(q"link:referenceArc")
              .plusAttribute(q"xlink:arcrole", "http://www.xbrl.org/2003/arcrole/concept-reference")
              .plusAttribute(q"xlink:from", "jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis_loc")
              .plusAttribute(q"xlink:to", "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
              .plusAttribute(q"xlink:type", "arc")
          }
      }
      .withoutNamespaceUndeclarations
      .elem

    nodebuilder.Document(Some(new URI("http://bogus-host/bogus-uri/bogus.xml")), elm)
  }
}
