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

package eu.cdevreeze.yaidom2.creationapi

import scala.collection.immutable

import eu.cdevreeze.yaidom2.core.EName

/**
 * Element creation API.
 *
 * @author Chris de Vreeze
 */
trait ElemCreationApi {

  type NodeType

  type ElemType <: NodeType

  def elem(name: EName, children: Seq[NodeType]): ElemType

  def elem(name: EName, attributes: immutable.Iterable[(EName, String)], children: Seq[NodeType]): ElemType

  def textElem(name: EName, txt: String): ElemType

  def textElem(name: EName, attributes: immutable.Iterable[(EName, String)], txt: String): ElemType

  def emptyElem(name: EName): ElemType

  def emptyElem(name: EName, attributes: immutable.Iterable[(EName, String)]): ElemType
}

object ElemCreationApi {

  /**
   * This creation API type, restricting NodeType and ElemType to the passed type parameters.
   *
   * @tparam N The node type
   * @tparam E The element type
   */
  type Aux[N, E] = ElemCreationApi {type NodeType = N; type ElemType = E}
}
