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

/**
 * The core concepts of yaidom2. For example, qualified names, expanded names, namespace declarations and in-scope namespaces.
 * These concepts form a "namespace theory", in which they are related to each other in a mathematically precise way.
 *
 * Import all members of this package in order to get friendly string interpolators for QNames and ENames
 * parsed from Strings. These are the "q" and "e" interpolators, respectively.
 *
 * The rest of yaidom2 depends on this package, and reversely, this package depends on nothing else in yaidom2.
 *
 * @author Chris de Vreeze
 */
package object core {

  /**
   * String interpolator for QNames parsed from strings.
   */
  implicit class QNameHelper(private val sc: StringContext) extends AnyVal {

    def q(args: Any*): QName = {
      require(args.forall(!_.toString.contains(':')), s"No colon allowed in the variables")

      // See https://docs.scala-lang.org/overviews/core/string-interpolation.html

      val strings = sc.parts.iterator
      val expressions = args.iterator

      val buf = new StringBuilder(strings.next())

      while (strings.hasNext) {
        buf.append(expressions.next())
        buf.append(strings.next())
      }

      QName.parse(buf.toString)
    }
  }

  /**
   * String interpolator for ENames parsed from strings.
   */
  implicit class ENameHelper(private val sc: StringContext) extends AnyVal {

    def e(args: Any*): EName = {
      require(args.forall(!_.toString.contains(':')), s"No colon allowed in the variables")

      // See https://docs.scala-lang.org/overviews/core/string-interpolation.html

      val strings = sc.parts.iterator
      val expressions = args.iterator

      val buf = new StringBuilder(strings.next())

      while (strings.hasNext) {
        buf.append(expressions.next())
        buf.append(strings.next())
      }

      EName.parse(buf.toString)
    }
  }
}
