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

package eu.cdevreeze.yaidom2.updateapi.internal

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.updateapi.UpdatableAttributeCarryingElemApi

/**
 * Abstract partially implemented UpdatableAttributeCarryingElemApi, for re-usable (but overridable) partial element implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not a stable as the purely abstract API.
 *
 * @author Chris de Vreeze
 */
trait AbstractUpdatableAttributeCarryingElem extends AbstractUpdatableElem with UpdatableAttributeCarryingElemApi {

  type ThisNode >: ThisElem <: ClarkNodes.Node

  type ThisElem <: AbstractUpdatableAttributeCarryingElem.Aux[ThisNode, ThisElem]

  def plusAttribute(attrName: EName, attrValue: String): ThisElem = {
    withAttributes(attributes.updated(attrName, attrValue))
  }

  def plusAttributeOption(attrName: EName, attrValueOption: Option[String]): ThisElem = {
    val newAttributes =
      attrValueOption.map(v => attributes.updated(attrName, v)).getOrElse(attributes)

    withAttributes(newAttributes)
  }

  def plusAttributes(newAttributes: SeqMap[EName, String]): ThisElem = {
    withAttributes(attributes.concat(newAttributes))
  }

  def minusAttribute(attrName: EName): ThisElem = {
    withAttributes(attributes.filterNot(_._1 == attrName))
  }
}

object AbstractUpdatableAttributeCarryingElem {

  type Aux[N, E] = AbstractUpdatableAttributeCarryingElem {type ThisNode = N; type ThisElem = E}
}
