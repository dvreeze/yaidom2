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

import eu.cdevreeze.yaidom2.core.EName

/**
 * Fast predicates on elements, to be used in queries.
 *
 * @author Chris de Vreeze
 */
package object predicates {

  val anyElem: Any => Boolean = {
    _ => true
  }

  def havingName[E, F <: ClarkElemFunctionsApi.Aux[E, _]](name: EName)(implicit ops: F): E => Boolean = {
    havingName(name.namespaceUriOption, name.localPart)
  }

  def havingName[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): E => Boolean = {
    elem =>
      val nsAsString = namespaceOption.getOrElse("")

      ops.namespaceAsString(elem) == nsAsString && ops.localName(elem) == localName
  }

  /**
   * Predicate returning true for elements having the given non-empty namespace and local name
   */
  def havingName[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): E => Boolean = {
    elem => ops.namespaceAsString(elem) == namespace && ops.localName(elem) == localName
  }

  /**
   * Predicate returning true for elements having no namespace but the given local name
   */
  def havingName[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): E => Boolean = {
    elem => ops.namespaceAsString(elem).isEmpty && ops.localName(elem) == localName
  }

  /**
   * Predicate returning true for elements having the given local name, ignoring the namespace, if any
   */
  def havingLocalName[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): E => Boolean = {
    e => ops.localName(e) == localName
  }
}
