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

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi._
import org.scalatest.FunSuite

/**
 * Test case testing the use of namespaces in Documents.
 *
 * Acknowledgments: This test uses the excellent article http://lenzconsulting.com/namespaces/ on "Understanding XML Namespaces".
 *
 * Note that this abstract test works with the "raw" type ClarkNodes.Elem. As a consequence, we cannot use the "select" API.
 *
 * @author Chris de Vreeze
 */
trait ClarkElemNamespaceQueryTest extends FunSuite {

  private val nsAtom = "http://www.w3.org/2005/Atom"
  private val nsXhtml = "http://www.w3.org/1999/xhtml"
  private val nsExamples = "http://xmlportfolio.com/xmlguild-examples"

  protected def getRootElem(fileName: String): ClarkNodes.Elem

  protected def circumventBugInXmlStack = false

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
      rootElem.findDescendantElemOrSelf(named(nsAtom, "feed"))
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
      expectedElemNames.filter(name => rootElem.findDescendantElemOrSelf(named(name)).nonEmpty)

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
      rootElem.filterDescendantElemsOrSelf(named(nsXhtml, "strong")).headOption.map(_.text).getOrElse("")
    }

    assertResult(Set(EName(nsXhtml, "strong"), EName(nsXhtml, "em"))) {
      rootElem.filterDescendantElems(named(nsXhtml, "div")).flatMap(_.findAllChildElems()).map(_.name).toSet
    }

    assertResult(Set(nsAtom, nsXhtml)) {
      rootElem.findAllDescendantElemsOrSelf.map(_.namespaceAsString).toSet
    }

    // JS-DOM does not see 2 attributes with the same local name, but one prefixed and the other without prefix,
    // so for JS-DOM we circumvent the following assertion.

    if (!circumventBugInXmlStack) {
      assertResult(Set("xhtml")) {
        rootElem.filterDescendantElemsOrSelf(named(nsAtom, "rights")).map(_.attrOption(None, "type").getOrElse("")).toSet
      }
    }

    assertResult(Set("silly")) {
      rootElem.filterDescendantElemsOrSelf(named(nsAtom, "rights")).map(_.attrOption(nsExamples, "type").getOrElse("")).toSet
    }
  }
}
