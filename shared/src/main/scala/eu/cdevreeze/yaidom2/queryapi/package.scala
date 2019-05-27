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

package eu.cdevreeze.yaidom2

import eu.cdevreeze.yaidom2.core.EName

/**
 * The query API of yaidom2.
 *
 * Most yaidom2 users do not directly use these query API traits, but use concrete element implementations instead.
 * Still, concrete element implementations mix in these query API traits, and new element implementations can be
 * invented that do the same.
 *
 * The API is designed in such a way that user code can mostly work with "raw" element types without having to specify the
 * concrete type parameters. This makes it easy to wrap element abstractions in "XML dialect" abstractions without needing
 * any generics in the latter, which greatly improves ease of use.
 *
 * The above-mentioned design has a simple abstract API in that the member type constraints are trivial F-bounded
 * polymorphism, without using self types etc. The implementations of that abstract API are mostly only in the concrete
 * element implementations (and not in generic implementation traits), even if that means that there is some code duplication.
 *
 * @author Chris de Vreeze
 */
package object queryapi {

  val anyElem: ElemApi => Boolean = {
    _ => true
  }

  def named(name: EName): ClarkElemApi => Boolean = {
    named(name.namespaceUriOption, name.localPart)
  }

  def named(namespaceOption: Option[String], localName: String): ClarkElemApi => Boolean = {
    elem =>
      val nsAsString = namespaceOption.getOrElse("")

      elem.namespaceAsString == nsAsString && elem.localName == localName
  }

  /**
   * Predicate returning true for elements having the given non-empty namespace and local name
   */
  def named(namespace: String, localName: String): ClarkElemApi => Boolean = {
    elem => elem.namespaceAsString == namespace && elem.localName == localName
  }

  /**
   * Predicate returning true for elements having no namespace but the given local name
   */
  def named(localName: String): ClarkElemApi => Boolean = {
    elem => elem.namespaceAsString.isEmpty && elem.localName == localName
  }

  /**
   * Predicate returning true for elements having the given local name, ignoring the namespace, if any
   */
  def havingLocalName(localName: String): ClarkElemApi => Boolean = {
    _.localName == localName
  }
}
