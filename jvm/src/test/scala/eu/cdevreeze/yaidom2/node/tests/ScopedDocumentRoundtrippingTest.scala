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

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.NamespacePrefixMapper
import eu.cdevreeze.yaidom2.core.PrefixedScope
import eu.cdevreeze.yaidom2.core.Scope
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
    val scope = Scope.from("link" -> LinkNs, "ref" -> RefNs, "xlink" -> XLinkNs, "xsi" -> XsiNs)
    val nsPrefixMapper = NamespacePrefixMapper.fromPrefixToNamespaceMapWithFallback(scope.prefixNamespaceMap)
    implicit val elemCreator = NodeBuilderCreator(nsPrefixMapper)

    import NodeBuilderCreator._
    import elemCreator._

    val elm: nodebuilder.Elem = emptyElem(EName(LinkNs, "namespace"), PrefixedScope.empty).creationApi
      .plusAttribute(EName(XsiNs, "schemaLocation"), "http://www.xbrl.org/2006/ref http://www.xbrl.org/2006/ref-2006-02-27.xsd")
      .plusChild {
        emptyElem(EName(LinkNs, "referenceLink"), PrefixedScope.empty).creationApi
          .plusAttribute(EName(XLinkNs, "role"), "http://www.xbrl.org/2003/role/link")
          .plusAttribute(EName(XLinkNs, "type"), "extended")
          .plusChild {
            emptyElem(EName(LinkNs, "loc"), PrefixedScope.empty).creationApi
              .plusAttribute(EName(XLinkNs, "href"), "jenv-bw2-axes.xsd#jenv-bw2-dim_LiabilitiesOtherAxis")
              .plusAttribute(EName(XLinkNs, "label"), "jenv-bw2-dim_LiabilitiesOtherAxis_loc")
              .plusAttribute(EName(XLinkNs, "type"), "locator")
              .underlying
          }
          .plusChild {
            emptyElem(EName(LinkNs, "loc"), PrefixedScope.empty).creationApi
              .plusAttribute(EName(XLinkNs, "href"), "jenv-bw2-axes.xsd#jenv-bw2-dim_LoansAdvancesGuaranteesAxis")
              .plusAttribute(EName(XLinkNs, "label"), "jenv-bw2-dim_LoansAdvancesGuaranteesAxis_loc")
              .plusAttribute(EName(XLinkNs, "type"), "locator")
              .underlying
          }
          .plusChild {
            emptyElem(EName(LinkNs, "loc"), PrefixedScope.empty).creationApi
              .plusAttribute(EName(XLinkNs, "href"), "jenv-bw2-axes.xsd#jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis")
              .plusAttribute(EName(XLinkNs, "label"), "jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis_loc")
              .plusAttribute(EName(XLinkNs, "type"), "locator")
              .underlying
          }
          .plusChild {
            emptyElem(EName(LinkNs, "reference"), PrefixedScope.empty).creationApi
              .plusAttribute(EName.parse("id"), "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
              .plusAttribute(EName(XLinkNs, "label"), "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
              .plusAttribute(EName(XLinkNs, "role"), "http://www.xbrl.org/2003/role/reference")
              .plusAttribute(EName(XLinkNs, "type"), "resource")
              .plusChild(textElem(EName(RefNs, "Article"), "383e", PrefixedScope.empty))
              .plusChild(textElem(EName(RefNs, "IssueDate"), "2019-01-01", PrefixedScope.empty))
              .plusChild(textElem(EName(RefNs, "Name"), "Burgerlijk wetboek boek 2", PrefixedScope.empty))
              .underlying
          }
          .plusChild {
            emptyElem(EName(LinkNs, "reference"), PrefixedScope.empty).creationApi
              .plusAttribute(EName.parse("id"), "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
              .plusAttribute(EName(XLinkNs, "label"), "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
              .plusAttribute(EName(XLinkNs, "role"), "http://www.xbrl.org/2003/role/reference")
              .plusAttribute(EName(XLinkNs, "type"), "resource")
              .plusChild(textElem(EName(RefNs, "Chapter"), "115", PrefixedScope.empty))
              .plusChild(textElem(EName(RefNs, "IssueDate"), "2019-01-01", PrefixedScope.empty))
              .plusChild(textElem(EName(RefNs, "Name"), "Richtlijnen voor de jaarverslaggeving", PrefixedScope.empty))
              .plusChild(textElem(EName(RefNs, "Paragraph"), "214", PrefixedScope.empty))
              .underlying
          }
          .plusChild {
            emptyElem(EName(LinkNs, "reference"), PrefixedScope.empty).creationApi
              .plusAttribute(EName.parse("id"), "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
              .plusAttribute(EName(XLinkNs, "label"), "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
              .plusAttribute(EName(XLinkNs, "role"), "http://www.xbrl.org/2003/role/reference")
              .plusAttribute(EName(XLinkNs, "type"), "resource")
              .plusChild(textElem(EName(RefNs, "Chapter"), "610", PrefixedScope.empty))
              .plusChild(textElem(EName(RefNs, "IssueDate"), "2019-01-01", PrefixedScope.empty))
              .plusChild(textElem(EName(RefNs, "Name"), "Richtlijnen voor de jaarverslaggeving", PrefixedScope.empty))
              .plusChild(textElem(EName(RefNs, "Paragraph"), "106", PrefixedScope.empty))
              .underlying
          }
          .plusChild {
            emptyElem(EName(LinkNs, "referenceArc"), PrefixedScope.empty).creationApi
              .plusAttribute(EName(XLinkNs, "arcrole"), "http://www.xbrl.org/2003/arcrole/concept-reference")
              .plusAttribute(EName(XLinkNs, "from"), "jenv-bw2-dim_LiabilitiesOtherAxis_loc")
              .plusAttribute(EName(XLinkNs, "to"), "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
              .plusAttribute(EName(XLinkNs, "type"), "arc")
              .underlying
          }
          .plusChild {
            emptyElem(EName(LinkNs, "referenceArc"), PrefixedScope.empty).creationApi
              .plusAttribute(EName(XLinkNs, "arcrole"), "http://www.xbrl.org/2003/arcrole/concept-reference")
              .plusAttribute(EName(XLinkNs, "from"), "jenv-bw2-dim_LoansAdvancesGuaranteesAxis_loc")
              .plusAttribute(EName(XLinkNs, "to"), "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
              .plusAttribute(EName(XLinkNs, "type"), "arc")
              .underlying
          }
          .plusChild {
            emptyElem(EName(LinkNs, "referenceArc"), PrefixedScope.empty).creationApi
              .plusAttribute(EName(XLinkNs, "arcrole"), "http://www.xbrl.org/2003/arcrole/concept-reference")
              .plusAttribute(EName(XLinkNs, "from"), "jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis_loc")
              .plusAttribute(EName(XLinkNs, "to"), "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
              .plusAttribute(EName(XLinkNs, "type"), "arc")
              .underlying
          }
          .underlying
      }
      .underlying

    nodebuilder.Document(Some(new URI("http://bogus-host/bogus-uri/bogus.xml")), elm)
  }
}
