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

package eu.cdevreeze.yaidom2.queryapi.steps

import eu.cdevreeze.yaidom2.core.EName

/**
 * Element step factory API.
 *
 * This API has been highly inspired by the Saxon 9.9 streaming API. Unlike the Saxon API, this API is Scala-centric
 * instead of Java-centric, this API limits itself to element nodes only, and it is generic.
 *
 * @author Chris de Vreeze
 */
trait ElemStepFactoryApi {

  type Elem <: Any

  type Step <: ElemStep[Elem]

  // Child axis, for element nodes only

  def childElems(p: Elem => Boolean): Step

  def childElems(): Step

  def childElems(localName: String): Step

  def childElems(namespace: String, localName: String): Step

  def childElems(namespaceOption: Option[String], localName: String): Step

  def childElems(ename: EName): Step

  // Descendant axis, for element nodes only

  def descendantElems(p: Elem => Boolean): Step

  def descendantElems(): Step

  def descendantElems(localName: String): Step

  def descendantElems(namespace: String, localName: String): Step

  def descendantElems(namespaceOption: Option[String], localName: String): Step

  def descendantElems(ename: EName): Step

  // Descendant-or-self axis, for element nodes only

  def descendantElemsOrSelf(p: Elem => Boolean): Step

  def descendantElemsOrSelf(): Step

  def descendantElemsOrSelf(localName: String): Step

  def descendantElemsOrSelf(namespace: String, localName: String): Step

  def descendantElemsOrSelf(namespaceOption: Option[String], localName: String): Step

  def descendantElemsOrSelf(ename: EName): Step

  // Descendant axis, short-circuiting, for element nodes only

  def topmostElems(p: Elem => Boolean): Step

  def topmostElems(localName: String): Step

  def topmostElems(namespace: String, localName: String): Step

  def topmostElems(namespaceOption: Option[String], localName: String): Step

  def topmostElems(ename: EName): Step

  // Descendant-or-self axis, short-circuiting, for element nodes only

  def topmostElemsOrSelf(p: Elem => Boolean): Step

  def topmostElemsOrSelf(localName: String): Step

  def topmostElemsOrSelf(namespace: String, localName: String): Step

  def topmostElemsOrSelf(namespaceOption: Option[String], localName: String): Step

  def topmostElemsOrSelf(ename: EName): Step
}

object ElemStepFactoryApi {

  type Aux[E, S] = ElemStepFactoryApi {
    type Elem = E
    type Step = S
  }
}
