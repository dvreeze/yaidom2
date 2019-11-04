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

package eu.cdevreeze.yaidom2.queryapi.elemstep

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.ClarkElemApi
import eu.cdevreeze.yaidom2.queryapi.ClarkElemStepFactoryApi
import eu.cdevreeze.yaidom2.queryapi.anyElem
import eu.cdevreeze.yaidom2.queryapi.havingLocalName
import eu.cdevreeze.yaidom2.queryapi.named

/**
 * ElemStep factory implementation for Clark elements.
 *
 * @author Chris de Vreeze
 */
// scalastyle:off number.of.methods
trait ClarkElemStepFactory extends ClarkElemStepFactoryApi {

  type ElemType <: ClarkElemApi.Aux[ElemType]

  // Child axis, for element nodes only

  final def childElems(p: ElemType => Boolean): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterChildElems(p) }
  }

  final def childElems(): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterChildElems(anyElem) }
  }

  final def childElems(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterChildElems(named(localName)) }
  }

  final def childElems(namespace: String, localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterChildElems(named(namespace, localName)) }
  }

  final def childElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterChildElems(named(namespaceOption, localName)) }
  }

  final def childElems(ename: EName): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterChildElems(named(ename)) }
  }

  final def childElemsIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterChildElems(havingLocalName(localName)) }
  }

  // Descendant axis, for element nodes only

  final def descendantElems(p: ElemType => Boolean): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElems(p) }
  }

  final def descendantElems(): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElems(anyElem) }
  }

  final def descendantElems(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElems(named(localName)) }
  }

  final def descendantElems(namespace: String, localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElems(named(namespace, localName)) }
  }

  final def descendantElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElems(named(namespaceOption, localName)) }
  }

  final def descendantElems(ename: EName): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElems(named(ename)) }
  }

  final def descendantElemsIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElems(havingLocalName(localName)) }
  }

  // Descendant-or-self axis, for element nodes only

  final def descendantElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElemsOrSelf(p) }
  }

  final def descendantElemsOrSelf(): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElemsOrSelf(anyElem) }
  }

  final def descendantElemsOrSelf(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElemsOrSelf(named(localName)) }
  }

  final def descendantElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElemsOrSelf(named(namespace, localName)) }
  }

  final def descendantElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElemsOrSelf(named(namespaceOption, localName)) }
  }

  final def descendantElemsOrSelf(ename: EName): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElemsOrSelf(named(ename)) }
  }

  final def descendantElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.filterDescendantElemsOrSelf(havingLocalName(localName)) }
  }

  // Descendant axis, short-circuiting, for element nodes only

  final def topmostElems(p: ElemType => Boolean): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElems(p) }
  }

  final def topmostElems(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElems(named(localName)) }
  }

  final def topmostElems(namespace: String, localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElems(named(namespace, localName)) }
  }

  final def topmostElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElems(named(namespaceOption, localName)) }
  }

  final def topmostElems(ename: EName): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElems(named(ename)) }
  }

  final def topmostElemsIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElems(havingLocalName(localName)) }
  }

  // Descendant-or-self axis, short-circuiting, for element nodes only

  final def topmostElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElemsOrSelf(p) }
  }

  final def topmostElemsOrSelf(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElemsOrSelf(named(localName)) }
  }

  final def topmostElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElemsOrSelf(named(namespace, localName)) }
  }

  final def topmostElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElemsOrSelf(named(namespaceOption, localName)) }
  }

  final def topmostElemsOrSelf(ename: EName): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElemsOrSelf(named(ename)) }
  }

  final def topmostElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { elem: ElemType => elem.findTopmostElemsOrSelf(havingLocalName(localName)) }
  }
}

object ClarkElemStepFactory {

  type Aux[E] = ClarkElemStepFactory { type ElemType = E }
}
