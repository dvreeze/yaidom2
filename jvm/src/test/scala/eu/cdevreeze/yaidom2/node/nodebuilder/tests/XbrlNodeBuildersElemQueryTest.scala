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

import eu.cdevreeze.yaidom2.node.nodebuilder
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.queryapi.tests.XbrlScopedElemQueryTest
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.XdmNode
import net.sf.saxon.s9api.streams.Steps.child
import net.sf.saxon.s9api.streams.Steps.descendant

import scala.collection.immutable.ArraySeq
import scala.jdk.StreamConverters._

class XbrlNodeBuildersElemQueryTest extends XbrlScopedElemQueryTest[nodebuilder.Elem] {

  protected def rootElem: nodebuilder.Elem = {
    nodebuilder.Document.from(saxonDocument).documentElement
  }

  protected val elemStepFactory: nodebuilder.NodeBuilderElemSteps.type = nodebuilder.NodeBuilderElemSteps

  private val processor = new Processor(false)

  import elemStepFactory._

  protected def inputXmlFileOnClasspath: String = "/test-xml/sample-xbrl-instance-2.xml"

  protected def saxonDocument: saxon.Document = {
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[XbrlNodeBuildersElemQueryTest].getResource(inputXmlFileOnClasspath).toURI)
    val doc = docBuilder.build(file)

    saxon.Document(doc)
  }

  protected def saxonRootElem: saxon.Elem = {
    saxonDocument.documentElement
  }

  test("testSemanticsOfStepsAgainstSaxon") {
    val dimensionalContexts =
      rootElem.select {
        descendantElems(XbrliNs, "context").where {
          _.select(childElems(XbrliNs, "entity") / descendantElems(XbrldiNs, "explicitMember")).nonEmpty
        }
      }

    val expectedDimensionalContexts =
      saxonRootElem.xdmNode
        .select {
          descendant(XbrliNs, "context").where { (e: XdmNode) =>
            e.select(child(XbrliNs, "entity").`then`(descendant(XbrldiNs, "explicitMember"))).exists
          }
        }
        .toScala(ArraySeq)
        .map(n => saxon.Elem(n))

    assertResult(true) {
      dimensionalContexts.size >= 10
    }
    assertResult(expectedDimensionalContexts.map(e => resolved.Elem.from(e))) {
      dimensionalContexts.map(e => resolved.Elem.from(e))
    }
  }
}
