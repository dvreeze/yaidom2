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
import eu.cdevreeze.yaidom2.node.jsdom.JsDomElemSteps
import eu.cdevreeze.yaidom2.node.jsdom.JsDomNodes
import eu.cdevreeze.yaidom2.queryapi.tests.XbrlBackingElemQueryTest
import eu.cdevreeze.yaidom2.testxml.SampleXbrlInstance
import org.scalajs.dom.experimental.domparser.DOMParser
import org.scalajs.dom.experimental.domparser.SupportedType

class XbrlJsDomElemQueryTest extends XbrlBackingElemQueryTest[JsDomNodes.Elem] {

  protected def rootElem: JsDomNodes.Elem = {
    val domParser = new DOMParser()
    JsDomDocument(domParser.parseFromString(SampleXbrlInstance.xmlString, SupportedType.`text/xml`)).documentElement
  }

  protected val elemStepFactory: JsDomElemSteps.type = JsDomElemSteps
}
