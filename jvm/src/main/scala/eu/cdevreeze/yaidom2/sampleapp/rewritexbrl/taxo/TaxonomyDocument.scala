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

package eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxo

import java.net.URI

import eu.cdevreeze.yaidom2.queryapi.oo.BackingDocumentApi
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.xpointer.XPointer

final class TaxonomyDocument private (val doc: BackingDocumentApi, val xpointerIndex: Map[XPointer, BackingNodes.Elem]) {

  def docUriOption: Option[URI] = doc.docUriOption

  def docUri: URI = docUriOption.getOrElse(new URI(""))

  def children: Seq[BackingNodes.CanBeDocumentChild] = doc.children

  def documentElement: BackingNodes.Elem = doc.documentElement
}

object TaxonomyDocument {

  def build(doc: BackingDocumentApi): TaxonomyDocument = {
    val xpointerIndex: Map[XPointer, BackingNodes.Elem] =
      doc.documentElement.findAllDescendantElemsOrSelf().map((e: BackingNodes.Elem) => XPointer.toXPointer(e) -> e).toMap

    new TaxonomyDocument(doc, xpointerIndex)
  }
}
