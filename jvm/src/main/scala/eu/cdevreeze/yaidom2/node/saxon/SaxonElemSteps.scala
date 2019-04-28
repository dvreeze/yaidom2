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

package eu.cdevreeze.yaidom2.node.saxon

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.oo.BackingElemStepFactoryApi
import eu.cdevreeze.yaidom2.queryapi.oo.anyElem
import eu.cdevreeze.yaidom2.queryapi.oo.havingLocalName
import eu.cdevreeze.yaidom2.queryapi.oo.havingName

/**
 * ElemStep factory API for Saxon elements.
 *
 * @author Chris de Vreeze
 */
// scalastyle:off number.of.methods
object SaxonElemSteps extends BackingElemStepFactoryApi {

  type ElemType = SaxonNodes.Elem

  // Child axis, for element nodes only

  def childElems(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterChildElems(p) }
  }

  def childElems(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterChildElems(anyElem) }
  }

  def childElems(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterChildElems(havingName(localName)) }
  }

  def childElems(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterChildElems(havingName(namespace, localName)) }
  }

  def childElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterChildElems(havingName(namespaceOption, localName)) }
  }

  def childElems(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterChildElems(havingName(ename)) }
  }

  def childElemsIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterChildElems(havingLocalName(localName)) }
  }

  // Descendant axis, for element nodes only

  def descendantElems(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElems(p) }
  }

  def descendantElems(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElems(anyElem) }
  }

  def descendantElems(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElems(havingName(localName)) }
  }

  def descendantElems(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElems(havingName(namespace, localName)) }
  }

  def descendantElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElems(havingName(namespaceOption, localName)) }
  }

  def descendantElems(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElems(havingName(ename)) }
  }

  def descendantElemsIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElems(havingLocalName(localName)) }
  }

  // Descendant-or-self axis, for element nodes only

  def descendantElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElemsOrSelf(p) }
  }

  def descendantElemsOrSelf(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElemsOrSelf(anyElem) }
  }

  def descendantElemsOrSelf(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElemsOrSelf(havingName(localName)) }
  }

  def descendantElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElemsOrSelf(havingName(namespace, localName)) }
  }

  def descendantElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElemsOrSelf(havingName(namespaceOption, localName)) }
  }

  def descendantElemsOrSelf(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElemsOrSelf(havingName(ename)) }
  }

  def descendantElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterDescendantElemsOrSelf(havingLocalName(localName)) }
  }

  // Descendant axis, short-circuiting, for element nodes only

  def topmostElems(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElems(p) }
  }

  def topmostElems(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElems(havingName(localName)) }
  }

  def topmostElems(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElems(havingName(namespace, localName)) }
  }

  def topmostElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElems(havingName(namespaceOption, localName)) }
  }

  def topmostElems(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElems(havingName(ename)) }
  }

  def topmostElemsIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElems(havingLocalName(localName)) }
  }

  // Descendant-or-self axis, short-circuiting, for element nodes only

  def topmostElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElemsOrSelf(p) }
  }

  def topmostElemsOrSelf(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElemsOrSelf(havingName(localName)) }
  }

  def topmostElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElemsOrSelf(havingName(namespace, localName)) }
  }

  def topmostElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElemsOrSelf(havingName(namespaceOption, localName)) }
  }

  def topmostElemsOrSelf(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElemsOrSelf(havingName(ename)) }
  }

  def topmostElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findTopmostElemsOrSelf(havingLocalName(localName)) }
  }

  // Parent axis, for element nodes only

  def parentElem(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(p).toIndexedSeq }
  }

  def parentElem(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(anyElem).toIndexedSeq }
  }

  def parentElem(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingName(localName)).toIndexedSeq }
  }

  def parentElem(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingName(namespace, localName)).toIndexedSeq }
  }

  def parentElem(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingName(namespaceOption, localName)).toIndexedSeq }
  }

  def parentElem(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingName(ename)).toIndexedSeq }
  }

  def parentElemIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.findParentElem(havingLocalName(localName)).toIndexedSeq }
  }

  // Ancestor axis, for element nodes only

  def ancestorElems(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(p) }
  }

  def ancestorElems(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(anyElem) }
  }

  def ancestorElems(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingName(localName)) }
  }

  def ancestorElems(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingName(namespace, localName)) }
  }

  def ancestorElems(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingName(namespaceOption, localName)) }
  }

  def ancestorElems(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingName(ename)) }
  }

  def ancestorElemsIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElems(havingLocalName(localName)) }
  }

  // Ancestor-or-self axis, for element nodes only

  def ancestorElemsOrSelf(p: ElemType => Boolean): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(p) }
  }

  def ancestorElemsOrSelf(): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(anyElem) }
  }

  def ancestorElemsOrSelf(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingName(localName)) }
  }

  def ancestorElemsOrSelf(namespace: String, localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingName(namespace, localName)) }
  }

  def ancestorElemsOrSelf(namespaceOption: Option[String], localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingName(namespaceOption, localName)) }
  }

  def ancestorElemsOrSelf(ename: EName): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingName(ename)) }
  }

  def ancestorElemsOrSelfIgnoringNamespace(localName: String): ElemStep[ElemType] = {
    { (elem: ElemType) => elem.filterAncestorElemsOrSelf(havingLocalName(localName)) }
  }
}
