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

package eu.cdevreeze.yaidom2.node.saxon.tests

import java.io.File

import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.queryapi.tests.ClarkElemNamespaceQueryTest
import net.sf.saxon.s9api.Processor

class SaxonElemNamespaceQueryTest extends ClarkElemNamespaceQueryTest {

  protected def getRootElem(fileName: String): saxon.Elem = {
    getSaxonDocument(fileName).documentElement
  }

  private val processor = new Processor(false)

  private def getSaxonDocument(fileName: String): saxon.Document = {
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[SaxonElemNamespaceQueryTest].getResource(s"/test-xml/$fileName").toURI)
    val doc = docBuilder.build(file)

    saxon.Document(doc)
  }
}
