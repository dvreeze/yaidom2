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
import eu.cdevreeze.yaidom2.queryapi.BackingElemApi
import eu.cdevreeze.yaidom2.queryapi.ClarkElemApi
import eu.cdevreeze.yaidom2.queryapi.predicates._

/**
 * Element step factory.
 *
 * This element step factory has been highly inspired by the Saxon 9.9 streaming API. Unlike the Saxon API, this API is Scala-centric
 * instead of Java-centric, this API limits itself to element nodes only, and it is generic.
 *
 * There is an important difference with the Saxon 9.9 API, in that step factories that take a local name expect the
 * name to have no namespace, instead of ignoring the namespace, if any. If the namespace must be ignored, consider
 * using methods that clearly indicate that they indeed ignore the namespace, if any.
 *
 * @author Chris de Vreeze
 */
object ElemSteps {

  // Child axis, for element nodes only

  def childElems[E <: Any with ClarkElemApi.Aux[E]](p: E => Boolean): ElemStep[E] = {
    { (elem: E) => elem.filterChildElems(p) }
  }

  def childElems[E <: Any with ClarkElemApi.Aux[E]](): ElemStep[E] = {
    { (elem: E) => elem.filterChildElems(anyElem) }
  }

  def childElems[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterChildElems(havingName(localName)) }
  }

  def childElems[E <: Any with ClarkElemApi.Aux[E]](namespace: String, localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterChildElems(havingName(namespace, localName)) }
  }

  def childElems[E <: Any with ClarkElemApi.Aux[E]](namespaceOption: Option[String], localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterChildElems(havingName(namespaceOption, localName)) }
  }

  def childElems[E <: Any with ClarkElemApi.Aux[E]](ename: EName): ElemStep[E] = {
    { (elem: E) => elem.filterChildElems(havingName(ename)) }
  }

  def childElemsIgnoringNamespace[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterChildElems(havingLocalName(localName)) }
  }

  // Descendant axis, for element nodes only

  def descendantElems[E <: Any with ClarkElemApi.Aux[E]](p: E => Boolean): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElems(p) }
  }

  def descendantElems[E <: Any with ClarkElemApi.Aux[E]](): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElems(anyElem) }
  }

  def descendantElems[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElems(havingName(localName)) }
  }

  def descendantElems[E <: Any with ClarkElemApi.Aux[E]](namespace: String, localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElems(havingName(namespace, localName)) }
  }

  def descendantElems[E <: Any with ClarkElemApi.Aux[E]](namespaceOption: Option[String], localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElems(havingName(namespaceOption, localName)) }
  }

  def descendantElems[E <: Any with ClarkElemApi.Aux[E]](ename: EName): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElems(havingName(ename)) }
  }

  def descendantElemsIgnoringNamespace[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElems(havingLocalName(localName)) }
  }

  // Descendant-or-self axis, for element nodes only

  def descendantElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](p: E => Boolean): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElemsOrSelf(p) }
  }

  def descendantElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElemsOrSelf(anyElem) }
  }

  def descendantElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElemsOrSelf(havingName(localName)) }
  }

  def descendantElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](namespace: String, localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElemsOrSelf(havingName(namespace, localName)) }
  }

  def descendantElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](namespaceOption: Option[String], localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElemsOrSelf(havingName(namespaceOption, localName)) }
  }

  def descendantElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](ename: EName): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElemsOrSelf(havingName(ename)) }
  }

  def descendantElemsOrSelfIgnoringNamespace[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterDescendantElemsOrSelf(havingLocalName(localName)) }
  }

  // Descendant axis, short-circuiting, for element nodes only

  def topmostElems[E <: Any with ClarkElemApi.Aux[E]](p: E => Boolean): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElems(p) }
  }

  def topmostElems[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElems(havingName(localName)) }
  }

  def topmostElems[E <: Any with ClarkElemApi.Aux[E]](namespace: String, localName: String): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElems(havingName(namespace, localName)) }
  }

  def topmostElems[E <: Any with ClarkElemApi.Aux[E]](namespaceOption: Option[String], localName: String): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElems(havingName(namespaceOption, localName)) }
  }

  def topmostElems[E <: Any with ClarkElemApi.Aux[E]](ename: EName): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElems(havingName(ename)) }
  }

  def topmostElemsIgnoringNamespace[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElems(havingLocalName(localName)) }
  }

  // Descendant-or-self axis, short-circuiting, for element nodes only

  def topmostElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](p: E => Boolean): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElemsOrSelf(p) }
  }

  def topmostElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElemsOrSelf(havingName(localName)) }
  }

  def topmostElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](namespace: String, localName: String): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElemsOrSelf(havingName(namespace, localName)) }
  }

  def topmostElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](namespaceOption: Option[String], localName: String): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElemsOrSelf(havingName(namespaceOption, localName)) }
  }

  def topmostElemsOrSelf[E <: Any with ClarkElemApi.Aux[E]](ename: EName): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElemsOrSelf(havingName(ename)) }
  }

  def topmostElemsOrSelfIgnoringNamespace[E <: Any with ClarkElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.findTopmostElemsOrSelf(havingLocalName(localName)) }
  }

  // Parent axis, for element nodes only

  def parentElem[E <: Any with BackingElemApi.Aux[E]](p: E => Boolean): ElemStep[E] = {
    { (elem: E) => elem.findParentElem(p).toIndexedSeq }
  }

  def parentElem[E <: Any with BackingElemApi.Aux[E]](): ElemStep[E] = {
    { (elem: E) => elem.findParentElem(anyElem).toIndexedSeq }
  }

  def parentElem[E <: Any with BackingElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.findParentElem(havingName(localName)).toIndexedSeq }
  }

  def parentElem[E <: Any with BackingElemApi.Aux[E]](namespace: String, localName: String): ElemStep[E] = {
    { (elem: E) => elem.findParentElem(havingName(namespace, localName)).toIndexedSeq }
  }

  def parentElem[E <: Any with BackingElemApi.Aux[E]](namespaceOption: Option[String], localName: String): ElemStep[E] = {
    { (elem: E) => elem.findParentElem(havingName(namespaceOption, localName)).toIndexedSeq }
  }

  def parentElem[E <: Any with BackingElemApi.Aux[E]](ename: EName): ElemStep[E] = {
    { (elem: E) => elem.findParentElem(havingName(ename)).toIndexedSeq }
  }

  def parentElemIgnoringNamespace[E <: Any with BackingElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.findParentElem(havingLocalName(localName)).toIndexedSeq }
  }

  // Ancestor axis, for element nodes only

  def ancestorElems[E <: Any with BackingElemApi.Aux[E]](p: E => Boolean): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElems(p) }
  }

  def ancestorElems[E <: Any with BackingElemApi.Aux[E]](): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElems(anyElem) }
  }

  def ancestorElems[E <: Any with BackingElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElems(havingName(localName)) }
  }

  def ancestorElems[E <: Any with BackingElemApi.Aux[E]](namespace: String, localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElems(havingName(namespace, localName)) }
  }

  def ancestorElems[E <: Any with BackingElemApi.Aux[E]](namespaceOption: Option[String], localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElems(havingName(namespaceOption, localName)) }
  }

  def ancestorElems[E <: Any with BackingElemApi.Aux[E]](ename: EName): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElems(havingName(ename)) }
  }

  def ancestorElemsIgnoringNamespace[E <: Any with BackingElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElems(havingLocalName(localName)) }
  }

  // Ancestor-or-self axis, for element nodes only

  def ancestorElemsOrSelf[E <: Any with BackingElemApi.Aux[E]](p: E => Boolean): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElemsOrSelf(p) }
  }

  def ancestorElemsOrSelf[E <: Any with BackingElemApi.Aux[E]](): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElemsOrSelf(anyElem) }
  }

  def ancestorElemsOrSelf[E <: Any with BackingElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElemsOrSelf(havingName(localName)) }
  }

  def ancestorElemsOrSelf[E <: Any with BackingElemApi.Aux[E]](namespace: String, localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElemsOrSelf(havingName(namespace, localName)) }
  }

  def ancestorElemsOrSelf[E <: Any with BackingElemApi.Aux[E]](namespaceOption: Option[String], localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElemsOrSelf(havingName(namespaceOption, localName)) }
  }

  def ancestorElemsOrSelf[E <: Any with BackingElemApi.Aux[E]](ename: EName): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElemsOrSelf(havingName(ename)) }
  }

  def ancestorElemsOrSelfIgnoringNamespace[E <: Any with BackingElemApi.Aux[E]](localName: String): ElemStep[E] = {
    { (elem: E) => elem.filterAncestorElemsOrSelf(havingLocalName(localName)) }
  }
}
