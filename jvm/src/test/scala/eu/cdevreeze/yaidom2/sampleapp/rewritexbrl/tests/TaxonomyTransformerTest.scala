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

package eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.tests

import java.io.File
import java.net.URI

import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.queryapi.oo.named
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.ENames
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxo
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxorewriter.TaxonomyTransformer
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal.ResolvedElemTransformations._
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.xpointer.XPointer
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite

class TaxonomyTransformerTest extends AnyFunSuite {

  private val processor = new Processor(false)

  test("testXPointer") {
    val file = "/test-xbrl-taxo/www.nltaxonomie.nl/nt13/jenv/20181212/dictionary/jenv-bw2-axes.xsd"
    val document = parseFile(file)
    val docUri = document.docUriOption.getOrElse(new URI(""))

    val inputTaxo = new taxo.Taxonomy(docUri, Map(docUri -> document))

    val elemUris: Set[URI] = inputTaxo.documentMap.values.flatMap { doc =>
      doc.xpointerIndex.keySet.map(xp => new URI(doc.docUri.getScheme, doc.docUri.getSchemeSpecificPart, xp.toString))
    }.toSet

    assertResult(inputTaxo.documentMap.values.flatMap(_.documentElement.findAllDescendantElemsOrSelf()).toSet) {
      elemUris.toSeq.map(u => inputTaxo.getElem(u)).toSet
    }

    assertResult(inputTaxo.documentMap.values.flatMap(_.documentElement.findAllDescendantElemsOrSelf()
      .map(e => new URI(e.docUri.getScheme, e.docUri.getSchemeSpecificPart, XPointer.toXPointer(e).toString))).toSet) {

      elemUris
    }
  }

  test("testTransformSchema") {
    val file = "/test-xbrl-taxo/www.nltaxonomie.nl/nt13/jenv/20181212/dictionary/jenv-bw2-axes.xsd"
    val document = parseFile(file)
    val docUri = document.docUri

    val inputTaxo = new taxo.Taxonomy(docUri, Map(docUri -> document))

    val taxoTransformer = new TaxonomyTransformer(inputTaxo)

    val outputDocument = taxoTransformer.transformSchema(document)

    assertResult(removeSchemaLocation(removeAnnotation(resolved.Elem.from(document.documentElement)))) {
      removeSchemaLocation(removeAnnotation(resolved.Elem.from(outputDocument.documentElement)))
    }

    assertResult(document.documentElement.findAllDescendantElemsOrSelf().size) {
      outputDocument.documentElement.findAllDescendantElemsOrSelf().size + 7
    }

    assertResult(Seq.empty) {
      outputDocument.documentElement.filterDescendantElemsOrSelf(named(ENames.XsAnnotationEName)).map(_.name)
    }

    assertResult(Seq.empty) {
      outputDocument.documentElement.filterDescendantElemsOrSelf(_.attrOption(ENames.SchemaLocationEName).isDefined).map(_.name)
    }

    assertResult(retainElementDeclarations(resolved.Elem.from(document.documentElement))) {
      retainElementDeclarations(resolved.Elem.from(outputDocument.documentElement))
    }
  }

  test("testTransformLinkbase") {
    val files = Seq(
      "/test-xbrl-taxo/www.nltaxonomie.nl/nt13/ezk/20181212/dictionary/ezk-ncgc-data.xsd",
      "/test-xbrl-taxo/www.nltaxonomie.nl/nt13/ezk/20181212/presentation/ezk-ncgc-abstracts.xsd",
      "/test-xbrl-taxo/www.nltaxonomie.nl/nt13/ezk/20181212/presentation/ezk-dutch-corporate-governance-code-2016-pre.xml"
    )

    val documents = files.map(parseFile)

    val inputTaxo = new taxo.Taxonomy(documents.last.docUri, documents.map(d => d.docUri -> d).toMap)

    val taxoTransformer = new TaxonomyTransformer(inputTaxo)

    val outputDocument = taxoTransformer.transformLinkbase(documents.last)

    val ezkDataNamespace = "http://www.nltaxonomie.nl/nt13/ezk/20181212/dictionary/ezk-ncgc-data"
    val ezkAbstractsNamespace = "http://www.nltaxonomie.nl/nt13/ezk/20181212/presentation/ezk-ncgc-abstracts"

    import TaxonomyTransformer._

    val locs = outputDocument.documentElement.filterDescendantElems(named(CLinkNamespace, "loc"))

    assertResult(true) {
      locs.size >= 30
    }
    assertResult(documents.last.documentElement.filterDescendantElems(named(ENames.LinkLocEName)).size) {
      locs.size
    }

    assertResult(Set(ezkDataNamespace, ezkAbstractsNamespace)) {
      locs.map(_.textAsResolvedQName).flatMap(_.namespaceUriOption).toSet
    }
  }

  private def parseFile(fileOnClasspath: String): taxo.TaxonomyDocument = {
    val docUri = classOf[TaxonomyTransformerTest].getResource(fileOnClasspath).toURI
    val docBuilder = processor.newDocumentBuilder()
    val doc = docBuilder.build(new File(docUri))
    taxo.TaxonomyDocument.build(saxon.Document(doc))
  }

  private def removeAnnotation(elem: resolved.Elem): resolved.Elem = {
    elem.transformDescendantElemsToNodeSeq { e =>
      if (e.name == ENames.XsAnnotationEName) Seq.empty else Seq(e)
    }
  }

  private def removeSchemaLocation(elem: resolved.Elem): resolved.Elem = {
    elem.transformDescendantElems { e =>
      if (e.attrOption(ENames.SchemaLocationEName).isDefined) e.copy(attributes = e.attributes - ENames.SchemaLocationEName) else e
    }
  }

  private def retainElementDeclarations(elem: resolved.Elem): resolved.Elem = {
    elem.transformChildElemsToNodeSeq { che =>
      if (che.name == ENames.XsElementEName) Seq(che) else Seq.empty
    }
  }
}
