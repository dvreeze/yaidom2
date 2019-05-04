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

package eu.cdevreeze.yaidom2.queryapi.fun

import java.net.URI

/**
 * Backing element function API. See `BackingElemApi`, but this is its non-OO functional counterpart. More precisely,
 * it is the non-OO functional counterpart of `BackingNodes.Elem`.
 *
 * @author Chris de Vreeze
 */
trait BackingElemFunctionsApi extends ScopedElemFunctionsApi {

  type ElemType

  def findParentElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def findParentElem(elem: ElemType): Option[ElemType]

  def filterAncestorElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAllAncestorElems(elem: ElemType): Seq[ElemType]

  def findAncestorElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterAncestorElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAllAncestorElemsOrSelf(elem: ElemType): Seq[ElemType]

  def findAncestorElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def findAllPrecedingSiblingElems(elem: ElemType): Seq[ElemType]

  /**
   * Returns the optional base URI, computed from the document URI, if any, and the XML base attributes of the
   * ancestors, if any.
   */
  def baseUriOption(elem: ElemType): Option[URI]

  /**
   * The base URI, defaulting to the empty URI if absent
   */
  def baseUri(elem: ElemType): URI

  /**
   * The optional document URI of the containing document, if any
   */
  def docUriOption(elem: ElemType): Option[URI]

  /**
   * The document URI, defaulting to the empty URI if absent
   */
  def docUri(elem: ElemType): URI

  /**
   * The root element
   */
  def rootElem(elem: ElemType): ElemType
}

object BackingElemFunctionsApi {

  type Aux[E, N] = BackingElemFunctionsApi {
    type ElemType = E
    type NodeType = N
  }
}
