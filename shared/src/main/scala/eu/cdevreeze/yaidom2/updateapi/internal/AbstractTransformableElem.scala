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

package eu.cdevreeze.yaidom2.updateapi.internal

import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.internal.AbstractClarkElem
import eu.cdevreeze.yaidom2.updateapi.TransformableElemApi

/**
 * Abstract partially implemented TransformableElemApi, for re-usable (but overridable) partial element implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not as stable as the purely abstract API.
 *
 * In concrete element classes extending this trait (directly or indirectly), strongly consider overriding all methods
 * that contain type member ThisElem anywhere in the method signature, by just calling the super-trait version of the method.
 * That would ensure that in those method signatures type member ThisElem has the correct concrete element type.
 *
 * @author Chris de Vreeze
 */
trait AbstractTransformableElem extends AbstractClarkElem with TransformableElemApi {

  type ThisNode >: ThisElem <: ClarkNodes.Node

  type ThisElem <: AbstractTransformableElem.Aux[ThisNode, ThisElem]

  def transformDescendantElemsOrSelf(f: ThisElem => ThisElem): ThisElem = {
    // Recursive calls
    f(transformChildElems(_.transformDescendantElemsOrSelf(f)))
  }

  def transformDescendantElems(f: ThisElem => ThisElem): ThisElem = {
    transformChildElems(_.transformDescendantElemsOrSelf(f))
  }

  def transformDescendantElemsOrSelfToNodeSeq(f: ThisElem => Seq[ThisNode]): Seq[ThisNode] = {
    // Recursive calls
    f(transformChildElemsToNodeSeq(_.transformDescendantElemsOrSelfToNodeSeq(f)))
  }

  def transformDescendantElemsToNodeSeq(f: ThisElem => Seq[ThisNode]): ThisElem = {
    transformChildElemsToNodeSeq(_.transformDescendantElemsOrSelfToNodeSeq(f))
  }
}

object AbstractTransformableElem {

  type Aux[N, E] = AbstractTransformableElem {type ThisNode = N; type ThisElem = E}
}
