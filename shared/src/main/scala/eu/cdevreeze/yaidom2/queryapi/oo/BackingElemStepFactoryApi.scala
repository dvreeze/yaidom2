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

package eu.cdevreeze.yaidom2.queryapi.oo

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ElemStep

/**
 * ElemStep factory API for Backing elements.
 *
 * @author Chris de Vreeze
 */
trait BackingElemStepFactoryApi extends ScopedElemStepFactoryApi {

  type ElemType <: BackingElemApi

  // Parent axis, for element nodes only

  def parentElem(p: ElemType => Boolean): ElemStep[ElemType]

  def parentElem(): ElemStep[ElemType]

  def parentElem(localName: String): ElemStep[ElemType]

  def parentElem(namespace: String, localName: String): ElemStep[ElemType]

  def parentElem(namespaceOption: Option[String], localName: String): ElemStep[ElemType]

  def parentElem(ename: EName): ElemStep[ElemType]

  def parentElemIgnoringNamespace(localName: String): ElemStep[ElemType]

  // Ancestor axis, for element nodes only

  def ancestorElems(p: ElemType => Boolean): ElemStep[ElemType]

  def ancestorElems(): ElemStep[ElemType]

  def ancestorElems(localName: String): ElemStep[ElemType]

  def ancestorElems(namespace: String, localName: String): ElemStep[ElemType]

  def ancestorElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType]

  def ancestorElems(ename: EName): ElemStep[ElemType]

  def ancestorElemsIgnoringNamespace(localName: String): ElemStep[ElemType]

  // Ancestor-or-self axis, for element nodes only

  def ancestorElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType]

  def ancestorElemsOrSelf(): ElemStep[ElemType]

  def ancestorElemsOrSelf(localName: String): ElemStep[ElemType]

  def ancestorElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType]

  def ancestorElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType]

  def ancestorElemsOrSelf(ename: EName): ElemStep[ElemType]

  def ancestorElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType]
}

object BackingElemStepFactoryApi {

  type Aux[E] = BackingElemStepFactoryApi {
    type ElemType = E
  }
}
