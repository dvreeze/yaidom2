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

package eu.cdevreeze.yaidom2.node.simple.propertytests

import java.io.File

import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.node.simple.SimpleNodes
import eu.cdevreeze.yaidom2.queryapi.propertytests.ElemApiSpecification
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.streams.Predicates._
import net.sf.saxon.s9api.streams.Steps._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class SimpleElemApiSpecification extends ElemApiSpecification[SimpleNodes.Elem] {

  implicit def arbitraryElem: Arbitrary[SimpleNodes.Elem] = SimpleElemApiSpecification.arbitraryElem

  implicit def arbitraryPred: Arbitrary[SimpleNodes.Elem => Boolean] = SimpleElemApiSpecification.arbitraryPred
}

object SimpleElemApiSpecification {

  private def getRootElem(path: String): SimpleNodes.Elem = {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[SimpleElemApiSpecification].getResource(path).toURI)
    val doc = docBuilder.build(file)

    SimpleNodes.Elem.from(SaxonNodes.Elem(doc.select(child(isElement)).findFirst().get))
  }

  private val rootElemPaths: Seq[String] = {
    Seq(
      "/test-xml/cars.xml",
      "/test-xml/feed1.xml",
      "/test-xml/feed2.xml",
      "/test-xml/feed3.xml",
      "/test-xml/sample-xbrl-instance.xml",
      "/test-xml/XMLSchema.xsd"
    )
  }

  val arbitraryElem: Arbitrary[SimpleNodes.Elem] = {
    val rootElems: Seq[SimpleNodes.Elem] = rootElemPaths.map(path => getRootElem(path))
    val allElems: Seq[SimpleNodes.Elem] = rootElems.flatMap(_.filterDescendantElemsOrSelf(_ => true))
    require(allElems.size >= 1000, s"Expected at least 1000 elements")

    Arbitrary(Gen.oneOf(allElems))
  }

  val arbitraryPred: Arbitrary[SimpleNodes.Elem => Boolean] = {
    Arbitrary(Gen.oneOf(Seq(predLocalName, predLocalNameSize, predLocalNameAllCapitals)))
  }

  private def predLocalName(e: SimpleNodes.Elem): Boolean = {
    e.name.localPart == e.localName
  }

  private def predLocalNameSize(e: SimpleNodes.Elem): Boolean = {
    e.name.localPart.size > 7
  }

  private def predLocalNameAllCapitals(e: SimpleNodes.Elem): Boolean = {
    e.name.localPart.forall(c => Character.isUpperCase(c))
  }
}
