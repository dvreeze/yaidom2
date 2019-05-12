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

import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.xpointer.XPointer

final class Taxonomy(val entrypointUris: Set[URI], val documentMap: Map[URI, TaxonomyDocument]) {

  def findElem(uri: URI): Option[BackingNodes.Elem] = {
    val docOption = documentMap.get(removeFragment(uri))

    docOption.flatMap { doc =>
      val xpointer: XPointer = XPointer.parse(uri.getFragment)
      doc.xpointerIndex.get(xpointer)
    }
  }

  def getElem(uri: URI): BackingNodes.Elem = {
    findElem(uri).getOrElse(sys.error(s"No element found at URI '$uri'"))
  }

  private def removeFragment(uri: URI): URI = {
    new URI(uri.getScheme, uri.getSchemeSpecificPart, null) // scalastyle:off null
  }
}
