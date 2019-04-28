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

package eu.cdevreeze.yaidom2.node

import java.io.File

import eu.cdevreeze.yaidom2.queryapi.oo.ClarkElemApi
import eu.cdevreeze.yaidom2.queryapi.propertytests.ElemApiSpecificationDataProvider
import net.sf.saxon.s9api.Processor
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Properties

abstract class DefaultElemApiSpecificationDataProvider[E <: ClarkElemApi.Aux[E]](name: String)
  extends Properties(name) with ElemApiSpecificationDataProvider[E] {

  import DefaultElemApiSpecificationDataProvider._

  implicit val arbitraryElem: Arbitrary[E] = {
    val rootElems: Seq[E] = rootElemPaths.map(path => getRootElemAsSaxonElem(path)).map(convertSaxonElemToElem)
    val allElems: Seq[E] = rootElems.flatMap(_.findAllDescendantElemsOrSelf())
    require(allElems.size >= 1000, s"Expected at least 1000 elements")

    Arbitrary(Gen.oneOf(Gen.oneOf(allElems), Gen.oneOf(rootElems)))
  }

  implicit val arbitraryPred: Arbitrary[E => Boolean] = {
    Arbitrary(
      Gen.oneOf(Seq(predLocalNameIsNameLocalPart, predLocalNameSizeGt7, predLocalNameContainsCapital, predLocalNameContainsNoCapital)))
  }

  protected def convertSaxonElemToElem(e: saxon.Elem): E

  private def predLocalNameIsNameLocalPart(e: E): Boolean = {
    e.name.localPart == e.localName
  }

  private def predLocalNameSizeGt7(e: E): Boolean = {
    e.name.localPart.size > 7
  }

  private def predLocalNameContainsCapital(e: E): Boolean = {
    e.name.localPart.exists(c => Character.isUpperCase(c))
  }

  private def predLocalNameContainsNoCapital(e: E): Boolean = {
    !predLocalNameContainsCapital(e)
  }
}

object DefaultElemApiSpecificationDataProvider {

  private def getRootElemAsSaxonElem(path: String): saxon.Elem = {
    val processor = new Processor(false)
    val docBuilder = processor.newDocumentBuilder()

    val file = new File(classOf[DefaultElemApiSpecificationDataProvider[_]].getResource(path).toURI)
    val doc = docBuilder.build(file)

    saxon.Document(doc).documentElement
  }

  private val rootElemPaths: Seq[String] = {
    Seq(
      "/test-xml/cars.xml",
      "/test-xml/feed1.xml",
      "/test-xml/feed2.xml",
      "/test-xml/feed3.xml",
      "/test-xml/sample-xbrl-instance.xml",
      "/test-xml/airportsGermany.xml",
      "/test-xml/miniXmlBaseTestFile.xml",
      "/test-xml/xmlBaseTestFile.xml"
    )
  }
}
