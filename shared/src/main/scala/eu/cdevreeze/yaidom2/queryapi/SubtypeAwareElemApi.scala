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

import scala.reflect.ClassTag

/**
 * Element API that makes querying for subtypes of the element type relatively easy.
 * This trait is typically used in "yaidom2 dialects", as part of the public query API, but also internally.
 *
 * @author Chris de Vreeze
 */
trait SubtypeAwareElemApi extends ElemApi {

  type ThisElem <: SubtypeAwareElemApi

  def filterChildElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B]

  def findAllChildElemsOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B]

  def findChildElemOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B]

  def findFirstChildElemOfType[B <: ThisElem](subtype: ClassTag[B]): Option[B]

  def filterDescendantElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B]

  def findAllDescendantElemsOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B]

  def findDescendantElemOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B]

  def filterDescendantElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B]

  def findAllDescendantElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B]

  def findDescendantElemOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B]

  def findTopmostElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B]

  def findTopmostElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B]
}

object SubtypeAwareElemApi {

  type Aux[E] = SubtypeAwareElemApi {
    type ThisElem = E
  }
}
