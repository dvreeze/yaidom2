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

package eu.cdevreeze.yaidom2.queryapi.oo.elemstep

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.oo.BackingElemApi
import eu.cdevreeze.yaidom2.queryapi.oo.BackingElemStepFactoryApi
import eu.cdevreeze.yaidom2.queryapi.oo.anyElem
import eu.cdevreeze.yaidom2.queryapi.oo.havingLocalName
import eu.cdevreeze.yaidom2.queryapi.oo.havingName

/**
 * ElemStep factory implementation for Backing elements.
 *
 * @author Chris de Vreeze
 */
// scalastyle:off number.of.methods
abstract class BackingElemStepFactory extends ScopedElemStepFactory with BackingElemStepFactoryApi {

  type ElemType <: BackingElemApi.Aux[ElemType]

  // Parent axis, for element nodes only

  final def parentElem(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(p).toIndexedSeq }
  }

  final def parentElem(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(anyElem).toIndexedSeq }
  }

  final def parentElem(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingName(localName)).toIndexedSeq }
  }

  final def parentElem(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingName(namespace, localName)).toIndexedSeq }
  }

  final def parentElem(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingName(namespaceOption, localName)).toIndexedSeq }
  }

  final def parentElem(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingName(ename)).toIndexedSeq }
  }

  final def parentElemIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingLocalName(localName)).toIndexedSeq }
  }

  // Ancestor axis, for element nodes only

  final def ancestorElems(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(p) }
  }

  final def ancestorElems(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(anyElem) }
  }

  final def ancestorElems(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingName(localName)) }
  }

  final def ancestorElems(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingName(namespace, localName)) }
  }

  final def ancestorElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingName(namespaceOption, localName)) }
  }

  final def ancestorElems(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingName(ename)) }
  }

  final def ancestorElemsIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingLocalName(localName)) }
  }

  // Ancestor-or-self axis, for element nodes only

  final def ancestorElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(p) }
  }

  final def ancestorElemsOrSelf(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(anyElem) }
  }

  final def ancestorElemsOrSelf(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingName(localName)) }
  }

  final def ancestorElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingName(namespace, localName)) }
  }

  final def ancestorElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingName(namespaceOption, localName)) }
  }

  final def ancestorElemsOrSelf(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingName(ename)) }
  }

  final def ancestorElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingLocalName(localName)) }
  }
}

object BackingElemStepFactory {

  type Aux[E] = BackingElemStepFactory { type ElemType = E }
}
