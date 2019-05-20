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

package eu.cdevreeze.yaidom2.queryapi.fun.internal

import java.net.URI

import scala.collection.immutable.ArraySeq

import eu.cdevreeze.yaidom2.queryapi.fun.BackingElemFunctionsApi

/**
 * Abstract partially implemented BackingElemFunctionsApi, for re-usable (but overridable) partial element function implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not a stable as the purely abstract API.
 *
 * @author Chris de Vreeze
 */
trait AbstractBackingElemFunctions extends AbstractScopedElemFunctions with BackingElemFunctionsApi {

  // BackingElemFunctionsApi

  def findParentElem(elem: ElemType): Option[ElemType] = {
    findParentElem(elem, _ => true)
  }

  def filterAncestorElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    toImmutableSeq(findParentElem(elem).toSeq).flatMap(e => filterAncestorElemsOrSelf(e, p))
  }

  def findAllAncestorElems(elem: ElemType): Seq[ElemType] = {
    filterAncestorElems(elem, _ => true)
  }

  def findAncestorElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    filterAncestorElems(elem, p).headOption
  }

  def filterAncestorElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    // Recursive calls
    toImmutableSeq(Seq(elem)).filter(p) ++ toImmutableSeq(findParentElem(elem).toSeq).flatMap(e => filterAncestorElemsOrSelf(e, p))
  }

  def findAllAncestorElemsOrSelf(elem: ElemType): Seq[ElemType] = {
    filterAncestorElemsOrSelf(elem, _ => true)
  }

  def findAncestorElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    filterAncestorElemsOrSelf(elem, p).headOption
  }

  def ownNavigationPathRelativeToRootElem(elem: ElemType): Seq[Int] = {
    def relativeNavigationPath(e: ElemType): Seq[Int] = {
      findParentElem(e).map { pe =>
        // Recursive call
        relativeNavigationPath(pe).appended(findAllPrecedingSiblingElems(e).size)
      }.getOrElse(IndexedSeq.empty)
    }

    relativeNavigationPath(elem).to(ArraySeq)
  }

  def baseUri(elem: ElemType): URI = {
    baseUriOption(elem).getOrElse(AbstractBackingElemFunctions.EmptyUri)
  }

  def docUri(elem: ElemType): URI = {
    docUriOption(elem).getOrElse(AbstractBackingElemFunctions.EmptyUri)
  }
}

object AbstractBackingElemFunctions {

  private[internal] val EmptyUri: URI = URI.create("")

  type Aux[N, E] = AbstractBackingElemFunctions {
    type NodeType = N
    type ElemType = E
  }
}
