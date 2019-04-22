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

package eu.cdevreeze.yaidom2.node.indexed.propertytests

import eu.cdevreeze.yaidom2.node.DefaultElemApiSpecificationDataProvider
import eu.cdevreeze.yaidom2.node.indexed.IndexedNodes
import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.node.simple.SimpleNodes
import eu.cdevreeze.yaidom2.queryapi.propertytests.BackingElemApiSpecification

class IndexedElemApiSpecification
  extends DefaultElemApiSpecificationDataProvider[IndexedNodes.Elem]("Indexed-BackingElemApi")
    with BackingElemApiSpecification[IndexedNodes.Node, IndexedNodes.Elem] {

  protected def convertSaxonElemToElem(e: SaxonNodes.Elem): IndexedNodes.Elem = {
    val underlyingElem = SimpleNodes.Elem.from(e)
    IndexedNodes.Elem.ofRoot(e.docUriOption, underlyingElem)
  }
}
