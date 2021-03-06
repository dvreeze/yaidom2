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

package eu.cdevreeze.yaidom2.node.indexed.tests

import eu.cdevreeze.yaidom2.creationapi.ScopedDocumentFactory
import eu.cdevreeze.yaidom2.node.indexed
import eu.cdevreeze.yaidom2.node.simple
import eu.cdevreeze.yaidom2.node.tests.ScopedDocumentRoundtrippingTest

class IndexedDocumentRoundtrippingTest extends ScopedDocumentRoundtrippingTest[indexed.Elem, indexed.Document] {

  protected def doc: indexed.Document = {
    indexed.Document.of(simple.Document.from(nodeBuilderDoc))
  }

  protected val documentFactory: ScopedDocumentFactory.Aux[indexed.Document] = indexed.Document.asScopedDocumentFactory
}
