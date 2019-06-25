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

package eu.cdevreeze.yaidom2.node.creationdsl.propertytests

import eu.cdevreeze.yaidom2.node.DefaultElemApiSpecificationDataProvider
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.creationdsl
import eu.cdevreeze.yaidom2.node.creationdsl.CreationDslElemSteps
import eu.cdevreeze.yaidom2.queryapi.propertytests.ScopedElemApiSpecification

class CreationDslElemApiSpecification
  extends DefaultElemApiSpecificationDataProvider[creationdsl.Elem]("CreationDsl-ScopedElemApi")
    with ScopedElemApiSpecification[creationdsl.Node, creationdsl.Elem] {

  protected def convertSaxonElemToElem(e: saxon.Elem): creationdsl.Elem = {
    creationdsl.Elem.from(e)
  }

  protected val elemStepFactory: CreationDslElemSteps.type = CreationDslElemSteps

  protected override def rootElemPaths: Seq[String] = {
    Seq(
      "/test-xml/taxonomyPackage.xml",
      "/test-xml/miniXmlBaseTestFile.xml",
      "/test-xml/cars.xml"
    )
  }
}
