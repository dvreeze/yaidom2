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

import eu.cdevreeze.yaidom2.creationapi.ScopedDocumentFactory
import eu.cdevreeze.yaidom2.node.saxon.SaxonProducers
import eu.cdevreeze.yaidom2.node.indexed
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.simple
import eu.cdevreeze.yaidom2.node.tests.ScopedDocumentRoundtrippingTest

class SaxonDocumentRoundtrippingTest extends ScopedDocumentRoundtrippingTest[saxon.Elem, saxon.Document] {

  protected def doc: saxon.Document = {
    SaxonProducers
      .documentProducer(processor)
      .from(indexed.Document.of(simple.Document.from(nodeBuilderDoc)))
      .ensuring(d =>
        resolved.Elem.from(d.documentElement).findAllDescendantElemsOrSelf.size ==
          resolved.Elem.from(nodeBuilderDoc.documentElement).findAllDescendantElemsOrSelf.size)
      .ensuring(_.docUriOption == nodeBuilderDoc.docUriOption.ensuring(_.nonEmpty))
  }

  protected val documentFactory: ScopedDocumentFactory.Aux[saxon.Document] =
    SaxonProducers.documentProducer(processor).asScopedDocumentFactory
}
