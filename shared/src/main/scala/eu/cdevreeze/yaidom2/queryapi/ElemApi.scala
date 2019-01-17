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

/**
 * Element API. It knows how to find elements by following one of the axes descendant-or-self, descendant
 * or child. It does not know anything about XML, so this API could even be used for non-XML tree-like objects.
 *
 * Concrete element implementations that (in)directly implement this interface should have efficient implementations
 * of the methods offered by this API. These methods can in turn be used to implement element step factories.
 *
 * @author Chris de Vreeze
 */
trait ElemApi extends Any {

  type ThisElem <: ElemApi

  def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem]

  def findChildElem(p: ThisElem => Boolean): Option[ThisElem]

  def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem]

  def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem]

  def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem]

  def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem]

  def findAllTopmostElems(p: ThisElem => Boolean): Seq[ThisElem]

  def findAllTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem]
}

object ElemApi {

  type Aux[E] = ElemApi {
    type ThisElem = E
  }
}
