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

package eu.cdevreeze.yaidom2.queryapi.ops

/**
 * Element function API. See `ElemApi`, but this is its non-OO functional counterpart.
 *
 * @author Chris de Vreeze
 */
trait ElemOpsApi {

  type Elem

  def filterChildElems(thisElem: Elem, p: Elem => Boolean): Seq[Elem]

  def findChildElem(thisElem: Elem, p: Elem => Boolean): Option[Elem]

  def filterDescendantElems(thisElem: Elem, p: Elem => Boolean): Seq[Elem]

  def findDescendantElem(thisElem: Elem, p: Elem => Boolean): Option[Elem]

  def filterDescendantElemsOrSelf(thisElem: Elem, p: Elem => Boolean): Seq[Elem]

  def findDescendantElemOrSelf(thisElem: Elem, p: Elem => Boolean): Option[Elem]

  def findTopmostElems(thisElem: Elem, p: Elem => Boolean): Seq[Elem]

  def findTopmostElemsOrSelf(thisElem: Elem, p: Elem => Boolean): Seq[Elem]
}
