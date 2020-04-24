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

package eu.cdevreeze.yaidom2.queryapi.internal

import eu.cdevreeze.yaidom2.queryapi.SubtypeAwareElemApi

import scala.reflect.ClassTag

/**
 * Abstract partially implemented SubtypeAwareElemApi, for re-usable (but overridable) partial element implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not as stable as the purely abstract API.
 *
 * In concrete element classes extending this trait (directly or indirectly), strongly consider overriding all methods
 * that contain type member ThisElem anywhere in the method signature, by just calling the super-trait version of the method.
 * That would ensure that in those method signatures type member ThisElem has the correct concrete element type.
 *
 * @author Chris de Vreeze
 */
trait AbstractSubtypeAwareElem extends SubtypeAwareElemApi {

  type ThisElem <: AbstractSubtypeAwareElem.Aux[ThisElem]

  // SubtypeAwareElemApi

  def filterChildElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] = {
    implicit val clsTag: ClassTag[B] = subtype

    filterChildElems {
      case e: B if p(e) => true
      case _            => false
    }.collect { case e: B => e }
  }

  def findAllChildElemsOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B] = {
    filterChildElemsOfType(subtype)(_ => true)
  }

  def findChildElemOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B] = {
    implicit val clsTag: ClassTag[B] = subtype

    findChildElem {
      case e: B if p(e) => true
      case _            => false
    }.collect { case e: B => e }
  }

  def findFirstChildElemOfType[B <: ThisElem](subtype: ClassTag[B]): Option[B] = {
    findChildElemOfType(subtype)(_ => true)
  }

  def filterDescendantElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] = {
    implicit val clsTag: ClassTag[B] = subtype

    filterDescendantElems {
      case e: B if p(e) => true
      case _            => false
    }.collect { case e: B => e }
  }

  def findAllDescendantElemsOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B] = {
    filterDescendantElemsOfType(subtype)(_ => true)
  }

  def findDescendantElemOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B] = {
    implicit val clsTag: ClassTag[B] = subtype

    findDescendantElem {
      case e: B if p(e) => true
      case _            => false
    }.collect { case e: B => e }
  }

  def filterDescendantElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] = {
    implicit val clsTag: ClassTag[B] = subtype

    filterDescendantElemsOrSelf {
      case e: B if p(e) => true
      case _            => false
    }.collect { case e: B => e }
  }

  def findAllDescendantElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B]): Seq[B] = {
    filterDescendantElemsOrSelfOfType(subtype)(_ => true)
  }

  def findDescendantElemOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Option[B] = {
    implicit val clsTag: ClassTag[B] = subtype

    findDescendantElemOrSelf {
      case e: B if p(e) => true
      case _            => false
    }.collect { case e: B => e }
  }

  def findTopmostElemsOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] = {
    implicit val clsTag: ClassTag[B] = subtype

    findTopmostElems {
      case e: B if p(e) => true
      case _            => false
    }.collect { case e: B => e }
  }

  def findTopmostElemsOrSelfOfType[B <: ThisElem](subtype: ClassTag[B])(p: B => Boolean): Seq[B] = {
    implicit val clsTag: ClassTag[B] = subtype

    findTopmostElemsOrSelf {
      case e: B if p(e) => true
      case _            => false
    }.collect { case e: B => e }
  }
}

object AbstractSubtypeAwareElem {

  type Aux[E] = AbstractSubtypeAwareElem { type ThisElem = E }
}
