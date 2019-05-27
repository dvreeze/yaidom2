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

package eu.cdevreeze.yaidom2.queryapi.oo.internal

import java.net.URI

import scala.collection.immutable.ArraySeq

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes

/**
 * Abstract partially implemented BackingNodes.Elem, for re-usable (but overridable) partial element implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not a stable as the purely abstract API.
 *
 * @author Chris de Vreeze
 */
trait AbstractBackingElem extends AbstractScopedElem with BackingNodes.Elem {

  type ThisElem <: AbstractBackingElem.Aux[ThisNode, ThisElem]

  import AbstractBackingElem.emptyUri
  import AbstractBackingElem.XmlBaseEName

  // BackingElemApi

  def findParentElem(): Option[ThisElem] = {
    findParentElem(_ => true)
  }

  def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    toImmutableSeq(findParentElem().toList).flatMap(_.filterAncestorElemsOrSelf(p))
  }

  def findAllAncestorElems(): Seq[ThisElem] = {
    filterAncestorElems(_ => true)
  }

  def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = {
    filterAncestorElems(p).headOption // TODO Improve performance!
  }

  def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
    // Recursive calls
    toImmutableSeq(Seq(self)).filter(p) ++ toImmutableSeq(findParentElem().toList).flatMap(_.filterAncestorElemsOrSelf(p))
  }

  def findAllAncestorElemsOrSelf(): Seq[ThisElem] = {
    filterAncestorElemsOrSelf(_ => true)
  }

  def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
    filterAncestorElemsOrSelf(p).headOption // TODO Improve performance!
  }

  def findAllPrecedingSiblingElems(): Seq[ThisElem] = {
    val parentElemOption = findParentElem()

    if (parentElemOption.isEmpty) {
      ArraySeq.empty
    } else {
      parentElemOption.get.findAllChildElems().takeWhile(_ != self).reverse
    }
  }

  def ownNavigationPathRelativeToRootElem: Seq[Int] = {
    def relativeNavigationPath(e: ThisElem): Seq[Int] = {
      e.findParentElem().map { pe =>
        // Recursive call
        relativeNavigationPath(pe).appended(e.findAllPrecedingSiblingElems().size)
      }.getOrElse(IndexedSeq.empty)
    }

    relativeNavigationPath(self).to(ArraySeq)
  }

  def baseUriOption: Option[URI] = {
    // Recursive call
    val parentBaseUriOption: Option[URI] =
      findParentElem().flatMap(_.baseUriOption).orElse(docUriOption)

    attrOption(XmlBaseEName).map(u => URI.create(u))
      .map(u => parentBaseUriOption.map(_.resolve(u)).getOrElse(u)).orElse(parentBaseUriOption)
  }

  def baseUri: URI = {
    baseUriOption.getOrElse(emptyUri)
  }

  def docUri: URI = {
    docUriOption.getOrElse(emptyUri)
  }
}

object AbstractBackingElem {

  type Aux[N, E] = AbstractBackingElem {type ThisNode = N; type ThisElem = E}

  private val emptyUri = URI.create("")

  private val XmlBaseEName = EName("http://www.w3.org/XML/1998/namespace", "base")
}
