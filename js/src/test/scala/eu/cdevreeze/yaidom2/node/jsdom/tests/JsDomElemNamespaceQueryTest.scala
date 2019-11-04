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
import eu.cdevreeze.yaidom2.testxml.Feed1
import eu.cdevreeze.yaidom2.testxml.Feed2
import eu.cdevreeze.yaidom2.testxml.Feed3
import org.scalajs.dom.experimental.domparser.DOMParser
import org.scalajs.dom.experimental.domparser.SupportedType

class JsDomElemNamespaceQueryTest extends ClarkElemNamespaceQueryTest {

  protected def getRootElem(fileName: String): JsDomNodes.Elem = {
    require(xmlFiles.contains(fileName), s"Expected one of ${xmlFiles.keySet.mkString("[", ", ", "]")}")

    val domParser = new DOMParser()
    JsDomDocument(domParser.parseFromString(xmlFiles(fileName), SupportedType.`text/xml`)).documentElement
  }

  // JS-DOM does not see 2 attributes with the same local name, but one prefixed and the other without prefix

  protected override def circumventBugInXmlStack = true

  private val xmlFiles: Map[String, String] =
    Map("feed1.xml" -> Feed1.xmlString, "feed2.xml" -> Feed2.xmlString, "feed3.xml" -> Feed3.xmlString)
}
