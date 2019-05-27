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
 * ElemStep factory API for Clark elements.
 *
 * This element step factory has been highly inspired by the Saxon 9.9 streaming API. Unlike the Saxon API, this API is Scala-centric
 * instead of Java-centric, this API limits itself to element nodes only, and it is generic.
 *
 * There is another important difference with the Saxon 9.9 API, in that step factories that take a local name expect the
 * name to have no namespace, instead of ignoring the namespace, if any. If the namespace must be ignored, consider
 * using methods that clearly indicate that they indeed ignore the namespace, if any.
 *
 * @author Chris de Vreeze
 */
// scalastyle:off number.of.methods
trait ClarkElemStepFactoryApi {

  type ElemType <: ClarkElemApi

  // Child axis, for element nodes only

  def childElems(p: ElemType => Boolean): ElemStep[ElemType]

  def childElems(): ElemStep[ElemType]

  def childElems(localName: String): ElemStep[ElemType]

  def childElems(namespace: String, localName: String): ElemStep[ElemType]

  def childElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType]

  def childElems(ename: EName): ElemStep[ElemType]

  def childElemsIgnoringNamespace(localName: String): ElemStep[ElemType]

  // Descendant axis, for element nodes only

  def descendantElems(p: ElemType => Boolean): ElemStep[ElemType]

  def descendantElems(): ElemStep[ElemType]

  def descendantElems(localName: String): ElemStep[ElemType]

  def descendantElems(namespace: String, localName: String): ElemStep[ElemType]

  def descendantElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType]

  def descendantElems(ename: EName): ElemStep[ElemType]

  def descendantElemsIgnoringNamespace(localName: String): ElemStep[ElemType]

  // Descendant-or-self axis, for element nodes only

  def descendantElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType]

  def descendantElemsOrSelf(): ElemStep[ElemType]

  def descendantElemsOrSelf(localName: String): ElemStep[ElemType]

  def descendantElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType]

  def descendantElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType]

  def descendantElemsOrSelf(ename: EName): ElemStep[ElemType]

  def descendantElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType]

  // Descendant axis, short-circuiting, for element nodes only

  def topmostElems(p: ElemType => Boolean): ElemStep[ElemType]

  def topmostElems(localName: String): ElemStep[ElemType]

  def topmostElems(namespace: String, localName: String): ElemStep[ElemType]

  def topmostElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType]

  def topmostElems(ename: EName): ElemStep[ElemType]

  def topmostElemsIgnoringNamespace(localName: String): ElemStep[ElemType]

  // Descendant-or-self axis, short-circuiting, for element nodes only

  def topmostElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType]

  def topmostElemsOrSelf(localName: String): ElemStep[ElemType]

  def topmostElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType]

  def topmostElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType]

  def topmostElemsOrSelf(ename: EName): ElemStep[ElemType]

  def topmostElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType]
}

object ClarkElemStepFactoryApi {

  type Aux[E] = ClarkElemStepFactoryApi {
    type ElemType = E
  }
}
