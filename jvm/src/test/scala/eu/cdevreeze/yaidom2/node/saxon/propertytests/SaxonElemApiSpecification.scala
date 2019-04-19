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

package eu.cdevreeze.yaidom2.node.saxon.propertytests

import java.io.File

import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.queryapi.propertytests.ElemApiSpecification
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.streams.Predicates._
import net.sf.saxon.s9api.streams.Steps._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class SaxonElemApiSpecification extends ElemApiSpecification[SaxonNodes.Elem]("Saxon-ElemApi") {

  implicit def arbitraryElem: Arbitrary[SaxonNodes.Elem] = SaxonElemApiSpecification.arbitraryElem

  implicit def arbitraryPred: Arbitrary[SaxonNodes.Elem => Boolean] = SaxonElemApiSpecification.arbitraryPred
}

object SaxonElemApiSpecification {

  private def getRootElem(path: String): SaxonNodes.Elem = {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[SaxonElemApiSpecification].getResource(path).toURI)
    val doc = docBuilder.build(file)

    SaxonNodes.Elem(doc.select(child(isElement)).findFirst().get)
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

  val arbitraryElem: Arbitrary[SaxonNodes.Elem] = {
    val rootElems: Seq[SaxonNodes.Elem] = rootElemPaths.map(path => getRootElem(path))
    val allElems: Seq[SaxonNodes.Elem] = rootElems.flatMap(_.filterDescendantElemsOrSelf(_ => true))
    require(allElems.size >= 1000, s"Expected at least 1000 elements")

    Arbitrary(Gen.oneOf(allElems))
  }

  val arbitraryPred: Arbitrary[SaxonNodes.Elem => Boolean] = {
    Arbitrary(Gen.oneOf(Seq(predLocalName, predLocalNameSize, predLocalNameAllCapitals)))
  }

  private def predLocalName(e: SaxonNodes.Elem): Boolean = {
    e.name.localPart == e.localName
  }

  private def predLocalNameSize(e: SaxonNodes.Elem): Boolean = {
    e.name.localPart.size > 7
  }

  private def predLocalNameAllCapitals(e: SaxonNodes.Elem): Boolean = {
    e.name.localPart.forall(c => Character.isUpperCase(c))
  }
}
