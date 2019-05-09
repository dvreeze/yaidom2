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

package eu.cdevreeze.yaidom2.node.jsdom.tests

import eu.cdevreeze.yaidom2.node.jsdom.JsDomDocument
import eu.cdevreeze.yaidom2.node.jsdom.JsDomNodes
import eu.cdevreeze.yaidom2.queryapi.tests.ClarkElemNamespaceQueryTest
import org.scalajs.dom.experimental.domparser.DOMParser
import org.scalajs.dom.experimental.domparser.SupportedType

class JsDomElemNamespaceQueryTest extends ClarkElemNamespaceQueryTest {

  protected def getRootElem(fileName: String): JsDomNodes.Elem = {
    require(xmlFiles.contains(fileName), s"Expected one of ${xmlFiles.keySet.mkString("[", ", ", "]")}")

    val domParser = new DOMParser()
    JsDomDocument(domParser.parseFromString(xmlFiles(fileName), SupportedType.`text/xml`)).documentElement
  }

  // JS-DOM does not see 2 attributes with the same local name, but one prefixed and the other without prefix

  protected override val circumventBugInXmlStack = true

  private val feed1 =
    """<feed xmlns="http://www.w3.org/2005/Atom"
      |      xmlns:xhtml="http://www.w3.org/1999/xhtml"
      |      xmlns:my="http://xmlportfolio.com/xmlguild-examples">
      |
      |    <title>Example Feed</title>
      |    <rights type="xhtml"
      |            my:type="silly">
      |        <xhtml:div>
      |            You may not read, utter, interpret, or otherwise
      |            <xhtml:strong>verbally process</xhtml:strong> the words
      |            contained in this feed without <xhtml:em>express written
      |            permission</xhtml:em> from the authors.
      |        </xhtml:div>
      |    </rights>
      |
      |    <!-- ... -->
      |
      |</feed>""".stripMargin

  private val feed2 =
    """<feed xmlns="http://www.w3.org/2005/Atom">
      |
      |    <title>Example Feed</title>
      |    <rights type="xhtml"
      |            example:type="silly"
      |            xmlns:example="http://xmlportfolio.com/xmlguild-examples">
      |        <div xmlns="http://www.w3.org/1999/xhtml">
      |            You may not read, utter, interpret, or otherwise
      |            <strong>verbally process</strong> the words
      |            contained in this feed without <em>express written
      |            permission</em> from the authors.
      |        </div>
      |    </rights>
      |
      |    <!-- ... -->
      |
      |</feed>""".stripMargin

  private val feed3 =
    """<feed xmlns="http://www.w3.org/2005/Atom"
      |      xmlns:xhtml="http://www.w3.org/1999/xhtml"
      |      xmlns:my="http://xmlportfolio.com/xmlguild-examples">
      |
      |    <title xmlns="http://www.w3.org/2005/Atom"
      |           xmlns:xhtml="http://www.w3.org/1999/xhtml"
      |           xmlns:my="http://xmlportfolio.com/xmlguild-examples">Example Feed</title>
      |    <rights type="xhtml"
      |            my:type="silly" xmlns="http://www.w3.org/2005/Atom"
      |            xmlns:xhtml="http://www.w3.org/1999/xhtml"
      |            xmlns:my="http://xmlportfolio.com/xmlguild-examples">
      |        <xhtml:div xmlns:xhtml="http://www.w3.org/1999/xhtml"
      |                   xmlns:my="http://xmlportfolio.com/xmlguild-examples">
      |            You may not read, utter, interpret, or otherwise
      |            <xhtml:strong>verbally process</xhtml:strong> the words
      |            contained in this feed without <xhtml:em>express written
      |            permission</xhtml:em> from the authors.
      |        </xhtml:div>
      |    </rights>
      |
      |    <!-- ... -->
      |
      |</feed>""".stripMargin

  private val xmlFiles: Map[String, String] = Map("feed1.xml" -> feed1, "feed2.xml" -> feed2, "feed3.xml" -> feed3)
}
