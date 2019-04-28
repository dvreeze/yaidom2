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

import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.saxon.SaxonElemSteps
import eu.cdevreeze.yaidom2.queryapi.tests.XbrlBackingElemQueryTest

class XbrlSaxonElemQueryTest extends XbrlBackingElemQueryTest[saxon.Elem] {

  protected def rootElem: saxon.Elem = {
    saxonRootElem
  }

  protected val elemStepFactory: SaxonElemSteps.type = SaxonElemSteps
}
