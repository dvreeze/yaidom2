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

package eu.cdevreeze.yaidom2.queryapi.tests

import java.io.File

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.oo._
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite

/**
 * Test case testing the use of namespaces in Documents.
 *
 * Acknowledgments: This test uses the excellent article http://lenzconsulting.com/namespaces/ on "Understanding XML Namespaces".
 *
 * Note that this abstract test works with the "raw" type ClarkNodes.Elem. As a consequence, we cannot use the "select" API.
 *
 * @author Chris de Vreeze
 */
abstract class ClarkElemNamespaceQueryTest extends AnyFunSuite {

  private val nsAtom = "http://www.w3.org/2005/Atom"
  private val nsXhtml = "http://www.w3.org/1999/xhtml"
  private val nsExamples = "http://xmlportfolio.com/xmlguild-examples"

  private val processor = new Processor(false)

  protected def getRootElem(fileName: String): ClarkNodes.Elem

  protected def getSaxonDocument(fileName: String): saxon.Document = {
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[ClarkElemNamespaceQueryTest].getResource(s"/test-xml/$fileName").toURI)
    val doc = docBuilder.build(file)

    saxon.Document(doc)
  }

  test("feed1") {
    testFeed("feed1.xml")
  }

  test("feed2") {
    testFeed("feed2.xml")
  }

  test("feed3") {
    testFeed("feed3.xml")
  }

  test("feedEquality") {
    val rootElem1 = getRootElem("feed1.xml")
    val rootElem2 = getRootElem("feed2.xml")
    val rootElem3 = getRootElem("feed3.xml")

    assertResult(resolved.Elem.from(rootElem1)) {
      resolved.Elem.from(rootElem2)
    }

    assertResult(resolved.Elem.from(rootElem1)) {
      resolved.Elem.from(rootElem3)
    }
  }

  // TODO More tests

  private def testFeed(feedFileName: String): Unit = {
    val rootElem = getRootElem(feedFileName)

    assertResult(Some(rootElem)) {
      rootElem.findDescendantElemOrSelf(havingName(nsAtom, "feed"))
    }

    val expectedElemNames = Set(
      EName(nsAtom, "feed"),
      EName(nsAtom, "title"),
      EName(nsAtom, "rights"),
      EName(nsXhtml, "div"),
      EName(nsXhtml, "strong"),
      EName(nsXhtml, "em")
    )

    val elemNamesFound: Set[EName] =
      expectedElemNames.filter(name => rootElem.findDescendantElemOrSelf(havingName(name)).nonEmpty)

    assertResult(expectedElemNames) {
      elemNamesFound
    }

    assertResult(expectedElemNames.filter(_.namespaceUriOption.contains(nsAtom))) {
      rootElem.filterDescendantElemsOrSelf(_.name.namespaceUriOption.contains(nsAtom)).map(_.name).toSet
    }

    assertResult(expectedElemNames.filter(_.namespaceUriOption.contains(nsXhtml))) {
      rootElem.filterDescendantElemsOrSelf(_.name.namespaceUriOption.contains(nsXhtml)).map(_.name).toSet
    }

    assertResult("verbally process") {
      rootElem.filterDescendantElemsOrSelf(havingName(nsXhtml, "strong")).headOption.map(_.text).getOrElse("")
    }

    assertResult(Set(EName(nsXhtml, "strong"), EName(nsXhtml, "em"))) {
      rootElem.filterDescendantElems(havingName(nsXhtml, "div")).flatMap(_.findAllChildElems()).map(_.name).toSet
    }

    assertResult(Set(nsAtom, nsXhtml)) {
      rootElem.findAllDescendantElemsOrSelf.map(_.namespaceAsString).toSet
    }

    assertResult(Set("xhtml")) {
      rootElem.filterDescendantElemsOrSelf(havingName(nsAtom, "rights")).map(_.attrOption(None, "type").getOrElse("")).toSet
    }

    assertResult(Set("silly")) {
      rootElem.filterDescendantElemsOrSelf(havingName(nsAtom, "rights")).map(_.attrOption(nsExamples, "type").getOrElse("")).toSet
    }
  }
}
