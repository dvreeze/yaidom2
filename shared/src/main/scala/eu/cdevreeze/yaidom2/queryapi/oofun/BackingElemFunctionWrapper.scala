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

package eu.cdevreeze.yaidom2.queryapi.oofun

import java.net.URI

import eu.cdevreeze.yaidom2.queryapi.fun.BackingElemFunctionsApi
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes

/**
 * BackingElemFunctionApi wrapper around BackingElemNodes.Elem.
 *
 * @author Chris de Vreeze
 */
trait BackingElemFunctionWrapper extends ScopedElemFunctionWrapper with BackingElemFunctionsApi {

  type ElemType <: BackingNodes.Elem.Aux[NodeType, ElemType]

  type NodeType >: ElemType <: BackingNodes.Node

  final def findParentElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    elem.findParentElem(p)
  }

  final def findParentElem(elem: ElemType): Option[ElemType] = {
    elem.findParentElem()
  }

  final def filterAncestorElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    elem.filterAncestorElems(p)
  }

  final def findAllAncestorElems(elem: ElemType): Seq[ElemType] = {
    elem.findAllAncestorElems()
  }

  final def findAncestorElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    elem.findAncestorElem(p)
  }

  final def filterAncestorElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    elem.filterAncestorElemsOrSelf(p)
  }

  final def findAllAncestorElemsOrSelf(elem: ElemType): Seq[ElemType] = {
    elem.findAllAncestorElemsOrSelf()
  }

  final def findAncestorElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    elem.findAncestorElemOrSelf(p)
  }

  final def findAllPrecedingSiblingElems(elem: ElemType): Seq[ElemType] = {
    elem.findAllPrecedingSiblingElems()
  }

  final def baseUriOption(elem: ElemType): Option[URI] = {
    elem.baseUriOption
  }

  final def baseUri(elem: ElemType): URI = {
    elem.baseUri
  }

  final def docUriOption(elem: ElemType): Option[URI] = {
    elem.docUriOption
  }

  final def docUri(elem: ElemType): URI = {
    elem.docUri
  }

  final def rootElem(elem: ElemType): ElemType = {
    elem.rootElem
  }
}
