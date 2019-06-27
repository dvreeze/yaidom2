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

package eu.cdevreeze.yaidom2.updateapi

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes

/**
 * Updatable element API, offering attribute updates as well.
 *
 * @author Chris de Vreeze
 */
trait UpdatableAttributeCarryingElemApi extends UpdatableElemApi {

  type ThisNode >: ThisElem <: ClarkNodes.Node

  type ThisElem <: UpdatableAttributeCarryingElemApi

  /**
   * Returns a copy in which the attributes have been replaced by the given collection of attributes.
   */
  def withAttributes(newAttributes: SeqMap[EName, String]): ThisElem

  /**
   * Returns a copy in which the given attribute has been added.
   */
  def plusAttribute(attrName: EName, attrValue: String): ThisElem

  /**
   * Returns a copy in which the given optional attribute has been added.
   */
  def plusAttributeOption(attrName: EName, attrValueOption: Option[String]): ThisElem

  /**
   * Returns a copy in which the given attributes have been added.
   */
  def plusAttributes(newAttributes: SeqMap[EName, String]): ThisElem

  /**
   * Returns a copy in which the given attribute has been removed, if any.
   */
  def minusAttribute(attrName: EName): ThisElem
}


object UpdatableAttributeCarryingElemApi {

  /**
   * This update API type, restricting Node and Elem to the passed type parameters.
   *
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[N, E] = UpdatableAttributeCarryingElemApi { type ThisNode = N; type ThisElem = E }
}
