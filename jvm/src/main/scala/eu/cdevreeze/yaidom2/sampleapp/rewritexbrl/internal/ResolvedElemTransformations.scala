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

package eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal

import eu.cdevreeze.yaidom2.node.resolved

/**
 * Element transformation API implemented for resolved elements.
 *
 * @author Chris de Vreeze
 */
object ResolvedElemTransformations extends ElemTransformationApi {

  type NodeType = resolved.Node

  type ElemType = resolved.Elem

  def transformChildElems(elem: ElemType, f: ElemType => ElemType): ElemType = {
    val children = elem.children.map {
      case e: resolved.Elem => f(e)
      case n => n
    }

    resolved.Elem(elem.name, elem.attributes, children)
  }

  def transformChildElemsToNodeSeq(elem: ElemType, f: ElemType => Seq[NodeType]): ElemType = {
    val children = elem.children.flatMap {
      case e: resolved.Elem => f(e)
      case n => Seq(n)
    }

    resolved.Elem(elem.name, elem.attributes, children)
  }

  def transformDescendantElemsOrSelf(elem: ElemType, f: ElemType => ElemType): ElemType = {
    f(transformChildElems(elem, e => transformDescendantElemsOrSelf(e, f)))
  }

  def transformDescendantElems(elem: ElemType, f: ElemType => ElemType): ElemType = {
    transformChildElems(elem, e => transformDescendantElemsOrSelf(e, f))
  }

  def transformDescendantElemsOrSelfToNodeSeq(elem: ElemType, f: ElemType => Seq[NodeType]): Seq[NodeType] = {
    f(transformChildElemsToNodeSeq(elem, e => transformDescendantElemsOrSelfToNodeSeq(e, f)))
  }

  def transformDescendantElemsToNodeSeq(elem: ElemType, f: ElemType => Seq[NodeType]): ElemType = {
    transformChildElemsToNodeSeq(elem, e => transformDescendantElemsOrSelfToNodeSeq(e, f))
  }

  implicit class Transformable(val elem: resolved.Elem) {

    val ElemType = resolved.Elem

    def transformChildElems(f: ElemType => ElemType): ElemType = {
      ResolvedElemTransformations.transformChildElems(elem, f)
    }

    def transformChildElemsToNodeSeq(f: ElemType => Seq[NodeType]): ElemType = {
      ResolvedElemTransformations.transformChildElemsToNodeSeq(elem, f)
    }

    def transformDescendantElemsOrSelf(f: ElemType => ElemType): ElemType = {
      ResolvedElemTransformations.transformDescendantElemsOrSelf(elem, f)
    }

    def transformDescendantElems(f: ElemType => ElemType): ElemType = {
      ResolvedElemTransformations.transformDescendantElems(elem, f)
    }

    def transformDescendantElemsOrSelfToNodeSeq(f: ElemType => Seq[NodeType]): Seq[NodeType] = {
      ResolvedElemTransformations.transformDescendantElemsOrSelfToNodeSeq(elem, f)
    }

    def transformDescendantElemsToNodeSeq(f: ElemType => Seq[NodeType]): ElemType = {
      ResolvedElemTransformations.transformDescendantElemsToNodeSeq(elem, f)
    }
  }
}
