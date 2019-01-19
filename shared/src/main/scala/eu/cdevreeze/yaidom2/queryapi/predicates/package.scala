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

import eu.cdevreeze.yaidom2.core.EName

/**
 * Fast predicates on Clark (and other) elements, to be used in queries.
 *
 * @author Chris de Vreeze
 */
package object predicates {

  val anyElem: ElemApi => Boolean = {
    _ => true
  }

  def havingName(name: EName): ClarkElemApi => Boolean = {
    havingName(name.namespaceUriOption, name.localPart)
  }

  def havingName(namespaceOption: Option[String], localName: String): ClarkElemApi => Boolean = {
    elem =>
      val nsAsString = namespaceOption.getOrElse("")

      elem.namespaceAsString == nsAsString && elem.localName == localName
  }

  /**
   * Predicate returning true for elements having the given non-empty namespace and local name
   */
  def havingName(namespace: String, localName: String): ClarkElemApi => Boolean = {
    elem => elem.namespaceAsString == namespace && elem.localName == localName
  }

  /**
   * Predicate returning true for elements having no namespace but the given local name
   */
  def havingName(localName: String): ClarkElemApi => Boolean = {
    elem => elem.namespaceAsString.isEmpty && elem.localName == localName
  }

  /**
   * Predicate returning true for elements having the given local name, ignoring the namespace, if any
   */
  def havingLocalName(localName: String): ClarkElemApi => Boolean = {
    _.localName == localName
  }
}
