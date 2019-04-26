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

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

/**
 * Element step, which is a function from elements to collections of elements. This is a monoid. See
 * https://typelevel.org/cats/typeclasses/monoid.html for an explanation of monoids.
 *
 * It is like a step in an XPath query, except that it concerns only element nodes.
 *
 * This API has been highly inspired by the Saxon 9.9 streaming API. Unlike the Saxon API, this API is Scala-centric
 * instead of Java-centric, and this API limits itself to element nodes only. It also returns no stream but a collection.
 *
 * @author Chris de Vreeze
 */
trait ElemStep[E] extends Function1[E, Seq[E]] {

  // Note that this trait must be a Java SAM interface, having only 1 abstract method, but at the same time
  // we need to provide implicit ClassTags wherever we create ArraySeq collections. Hence we provide these
  // ClassTags in every method that needs them, instead of via an extra abstract method (which would violate the
  // SAM interface constraint).

  /**
   * Associative operation to combine 2 steps.
   */
  final def concat(step: ElemStep[E]): ElemStep[E] = {
    { elem => this(elem).flatMap(step) }
  }

  /**
   * Alias for method concat.
   */
  // scalastyle:off method.name
  final def /(step: ElemStep[E]): ElemStep[E] = {
    concat(step)
  }

  final def where(p: E => Boolean): ElemStep[E] = {
    { elem => this(elem).filter(p) }
  }

  final def cat(step: ElemStep[E]): ElemStep[E] = {
    { elem => this(elem) ++ step(elem) }
  }

  final def first(implicit clsTag: ClassTag[E]): ElemStep[E] = {
    { elem => ArraySeq(this(elem).head) }
  }

  final def firstOption(implicit clsTag: ClassTag[E]): ElemStep[E] = {
    { elem => this(elem).headOption.to(ArraySeq) }
  }
}

object ElemStep {

  /**
   * The empty value of the ElemStep monoid.
   */
  def empty[E: ClassTag]: ElemStep[E] = {
    { elem => ArraySeq(elem) }
  }
}
