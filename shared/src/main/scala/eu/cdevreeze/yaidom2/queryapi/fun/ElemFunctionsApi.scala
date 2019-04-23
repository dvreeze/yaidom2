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

/**
 * Element function API. See `ElemApi`, but this is its non-OO functional counterpart.
 *
 * @author Chris de Vreeze
 */
trait ElemFunctionsApi {

  type ElemType

  def filterChildElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAllChildElems(elem: ElemType): Seq[ElemType]

  def findChildElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterDescendantElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAllDescendantElems(elem: ElemType): Seq[ElemType]

  def findDescendantElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterDescendantElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAllDescendantElemsOrSelf(elem: ElemType): Seq[ElemType]

  def findDescendantElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def findTopmostElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findTopmostElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]
}

object ElemFunctionsApi {

  type Aux[E] = ElemFunctionsApi {
    type ElemType = E
  }
}
