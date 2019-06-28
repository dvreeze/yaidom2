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

package eu.cdevreeze.yaidom2.node.nodebuilder.propertytests

import eu.cdevreeze.yaidom2.node.DefaultElemApiSpecificationDataProvider
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.nodebuilder
import eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders
import eu.cdevreeze.yaidom2.queryapi.propertytests.ScopedElemApiSpecification

class NodeBuildersElemApiSpecification
  extends DefaultElemApiSpecificationDataProvider[nodebuilder.Elem]("NodeBuilders-ScopedElemApi")
    with ScopedElemApiSpecification[nodebuilder.Node, nodebuilder.Elem] {

  protected def convertSaxonElemToElem(e: saxon.Elem): nodebuilder.Elem = {
    nodebuilder.Elem.from(e)
  }

  protected val elemStepFactory: NodeBuilders.ElemSteps.type = NodeBuilders.ElemSteps

  protected override def rootElemPaths: Seq[String] = {
    Seq(
      "/test-xml/taxonomyPackage.xml",
      "/test-xml/miniXmlBaseTestFile.xml",
      "/test-xml/cars.xml",
      "/test-xml/sample-xbrl-instance-2.xml"
    )
  }
}
