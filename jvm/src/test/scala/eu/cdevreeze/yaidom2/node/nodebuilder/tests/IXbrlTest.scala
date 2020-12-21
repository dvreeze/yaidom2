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

package eu.cdevreeze.yaidom2.node.nodebuilder.tests

import java.io.File
import java.time.LocalDate

import eu.cdevreeze.yaidom2.core._
import eu.cdevreeze.yaidom2.node.nodebuilder
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.saxon.SaxonProducers
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes
import eu.cdevreeze.yaidom2.queryapi.named
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ListMap

/**
 * Test illustrating the use of "nodebuilder" elements to create iXBRL
 * (see https://www.xbrl.org/the-standard/what/ixbrl/).
 *
 * @author Chris de Vreeze
 */
class IXbrlTest extends AnyFunSuite {

  private val processor = new Processor(false)

  // A file from the iXBRL conformance suite, edited to close the meta tag
  private def inputXhtmlFileOnClasspath: String = "/test-xml/Basic Inline XBRL Example.html"

  private def saxonXhtmlDocument: saxon.Document = {
    val file = new File(classOf[IXbrlTest].getResource(inputXhtmlFileOnClasspath).toURI)
    SaxonProducers.parser(processor).parse(file)
  }

  private val XhtmlNs = "http://www.w3.org/1999/xhtml"
  private val IxNs = "http://www.xbrl.org/2013/inlineXBRL"
  private val IxtNs = "http://www.xbrl.org/inlineXBRL/transformation/2010-04-20"
  private val XbrliNs = "http://www.xbrl.org/2003/instance"
  private val LinkNs = "http://www.xbrl.org/2003/linkbase"
  private val XLinkNs = "http://www.w3.org/1999/xlink"
  private val XbrldiNs = "http://xbrl.org/2006/xbrldi"
  private val Iso4217Ns = "http://www.xbrl.org/2003/iso4217"
  private val XsiNs = "http://www.w3.org/2001/XMLSchema-instance"

  // To create XHTML iXBRL documents, we need the XHTML namespace as default namespace,
  // and common iXBRL and related namespaces with their known prefixes. Hence the StableScope
  // below as "starting point". Note that during (iXBRL) document creation, we can add new
  // prefix-namespace pairs when needed, but the prefix-namespace pairs below, including the
  // default namespace, cannot change in a "compatible" way.

  private val knownStableScope: StableScope = StableScope.from(
    "" -> XhtmlNs,
    "ix" -> IxNs,
    "ixt" -> IxtNs,
    "link" -> LinkNs,
    "xbrli" -> XbrliNs,
    "xlink" -> XLinkNs,
    "xbrldi" -> XbrldiNs,
    "iso4217" -> Iso4217Ns,
    "xsi" -> XsiNs,
  )

  private val elemCreator: nodebuilder.NodeBuilderCreator = nodebuilder.NodeBuilderCreator(knownStableScope)

  import elemCreator._

  test("testCreateIxbrlContent") {
    // See inlineXBRL-1.1-conformanceSuite-2020-04-08/tests/nonNumeric/PASS-element-ix-nonNumeric-complete.html

    val xbrliContext1: nodebuilder.Elem = makeXbrliContext("DUR-1", LocalDate.parse("2005-12-31"), LocalDate.parse("2006-12-31"))
    val xbrliContext2: nodebuilder.Elem = makeXbrliContext("NFC1", LocalDate.parse("2005-01-01"), LocalDate.parse("2005-12-31"))

    val xbrliUnit1: nodebuilder.Elem = makeXbrliUnit("u1", q"iso4217:GBP", StableScope.empty)
    val xbrliUnit2: nodebuilder.Elem = makeXbrliUnit("ID-PURE", q"xbrli:pure", StableScope.empty)
    val xbrliUnit3: nodebuilder.Elem = makeXbrliUnit("GBP", q"iso4217:GBP", StableScope.empty)

    val xhtml: nodebuilder.Elem =
      emptyElem(q"html")
        .plusChildElem(
          elem(
            q"head",
            Seq(
              emptyElem(q"meta", ListMap(q"content" -> "text/html; charset=UTF-8", q"http-equiv" -> "Content-Type")).elem,
              textElem(q"title", "Basic Inline XBRL Example").elem
            )
          )
        )
        .plusChildElem(
          emptyElem(q"body", ListMap(q"xml:lang" -> "en"))
            .plusChildElem(
              emptyElem(q"div", ListMap(q"style" -> "display:none"))
                .plusChildElem(
                  emptyElem(q"ix:header")
                    .plusChildElem(
                      elem(
                        q"ix:references",
                        Seq(emptyElem(
                          q"link:schemaref",
                          ListMap(q"xlink:href" -> "../../schemas/ch/pt/2004-12-01/uk-gaap-pt-2004-12-01.xsd", q"xlink:type" -> "simple")).elem)
                      )
                    )
                    .plusChildElem(
                      emptyElem(q"ix:resources")
                        .plusChild(xbrliContext1)
                        .plusChild(xbrliContext2)
                        .plusChild(xbrliUnit1)
                        .plusChild(xbrliUnit2)
                        .plusChild(xbrliUnit3)
                    )
                )
            )
            .plusChildElem(
              elem(
                q"ix:nonnumeric",
                ListMap(q"contextref" -> "DUR-1", q"name" -> "pt:DescriptionAddressAssociate"),
                Seq(
                  elem(
                    q"b",
                    Seq(
                      nodebuilder.NodeBuilders.Text("   A string of text.   "),
                      elem(
                        q"ix:exclude",
                        Seq(
                          textElem(q"i", "   A number. 1,234,456.78   ").elem
                        )
                      ).elem,
                      nodebuilder.NodeBuilders.Text("   More text>   "),
                    )
                  ).elem
                )
              ).usingExtraScope(StableScope.from("pt" -> "http://www.xbrl.org/uk/fr/gaap/pt/2004-12-01"))
            )
        )
        .havingSameScopeInDescendantsOrSelf
        .elem

    assertResult(true) {
      xhtml.findAllDescendantElemsOrSelf.map(_.stableScope).distinct.sizeIs == 1
    }
    assertResult(1) {
      xhtml.filterDescendantElems(named(IxNs, "nonnumeric")).size
    }

    // Compare created XHTML with original (from the iXBRL conformance suite)

    assertResult(saxonXhtmlDocument.documentElement.filterDescendantElems(named(XbrliNs, "context")).map(toComparableResolvedElem)) {
      xhtml.filterDescendantElems(named(XbrliNs, "context")).map(toComparableResolvedElem)
    }

    assertResult(saxonXhtmlDocument.documentElement.filterDescendantElems(named(XbrliNs, "unit")).map(toComparableResolvedElem)) {
      xhtml.filterDescendantElems(named(XbrliNs, "unit")).map(toComparableResolvedElem)
    }

    assertResult(
      saxonXhtmlDocument.documentElement
        .filterDescendantElems(named(IxNs, "nonnumeric"))
        .map(toComparableResolvedElem)) {
      xhtml
        .filterDescendantElems(named(IxNs, "nonnumeric"))
        .map(toComparableResolvedElem)
    }

    assertResult(toComparableResolvedElem(saxonXhtmlDocument.documentElement)) {
      toComparableResolvedElem(xhtml)
    }
  }

  private def makeXbrliContext(id: String, startDate: LocalDate, endDate: LocalDate): nodebuilder.Elem = {
    emptyElem(q"xbrli:context")
      .plusAttribute(q"id", id)
      .plusChildElem(
        elem(
          q"xbrli:entity",
          Seq(textElem(q"xbrli:identifier", ListMap(q"scheme" -> "test"), "Test Co 1").elem)
        )
      )
      .plusChildElem(
        elem(
          q"xbrli:period",
          Seq(
            textElem(q"xbrli:startdate", startDate.toString).elem,
            textElem(q"xbrli:enddate", endDate.toString).elem,
          )
        )
      )
      .elem
  }

  private def makeXbrliUnit(id: String, measure: QName, neededScopeForMeasure: StableScope): nodebuilder.Elem = {
    require(knownStableScope.appendNonConflictingScope(neededScopeForMeasure).resolveQNameOption(measure).nonEmpty)

    emptyElem(q"xbrli:unit")
      .plusAttribute(q"id", id)
      .plusChildElem(textElem(q"xbrli:measure", measure.toString))
      .usingExtraScope(neededScopeForMeasure)
      .elem
  }

  private def toComparableResolvedElem(elm: ScopedNodes.Elem): resolved.Elem = {
    resolved.Elem
      .from(elm)
      .transformDescendantElemsOrSelf {
        case e @ resolved.Elem(EName(Some(XhtmlNs), "html"), _, _) =>
          e.copy(attributes = e.attributes.filter(kv => !Set(e"version", e"{$XsiNs}schemalocation").contains(kv._1)))
        case e => e
      }
      .transformDescendantElemsOrSelf { e =>
        e.copy(children = e.children.filter {
          case resolved.Text(t) if t.trim.isEmpty => false
          case _                                  => true
        })
      }
      .coalesceAndNormalizeAllText
      .removeAllInterElementWhitespace
  }
}
