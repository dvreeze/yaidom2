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

package eu.cdevreeze.yaidom2.creationapi

import eu.cdevreeze.yaidom2.queryapi.oo.ClarkNodes

/**
 * Factory APIs for Clark nodes.
 *
 * @author Chris de Vreeze
 */
object ClarkNodeFactories {

  trait NodeFactory {

    type TargetNodeType <: ClarkNodes.Node

    def from(node: ClarkNodes.Node): TargetNodeType
  }

  trait ElemFactory {

    type TargetElemType <: ClarkNodes.Elem

    def from(elem: ClarkNodes.Elem): TargetElemType
  }
}
