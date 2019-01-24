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

package eu.cdevreeze.yaidom2.queryapi

import java.net.URI

/**
 * So-called "backing" element API. It extends trait `ScopedElemApi`, but it is also aware of ancestor elements, document
 * URI, base URI etc. It is therefore fit as backing element of yaidom2 "dialects", where we often need to query ancestor
 * elements, base URI, etc. Hence the name `BackingElemApi`. This API knows about XML elements, but it does not know about
 * nodes other than element nodes.
 *
 * @author Chris de Vreeze
 */
trait BackingElemApi extends ScopedElemApi {

  type ThisElem <: BackingElemApi

  def findParentElem(p: ThisElem => Boolean): Option[ThisElem]

  def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem]

  def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem]

  def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem]

  def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem]

  /**
   * Returns the optional base URI, computed from the document URI, if any, and the XML base attributes of the
   * ancestors, if any.
   */
  def baseUriOption: Option[URI]

  /**
   * The base URI, defaulting to the empty URI if absent
   */
  def baseUri: URI

  /**
   * The optional document URI of the containing document, if any
   */
  def docUriOption: Option[URI]

  /**
   * The document URI, defaulting to the empty URI if absent
   */
  def docUri: URI

  /**
   * The root element
   */
  def rootElem: ThisElem
}

object BackingElemApi {

  type Aux[E] = BackingElemApi {
    type ThisElem = E
  }
}
