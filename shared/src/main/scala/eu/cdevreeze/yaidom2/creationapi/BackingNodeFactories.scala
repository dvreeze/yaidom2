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

import java.net.URI

import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes

/**
 * Factory APIs for Backing nodes.
 *
 * @author Chris de Vreeze
 */
object BackingNodeFactories {

  trait NodeFactory {

    type TargetNodeType <: BackingNodes.Node

    def from(node: BackingNodes.Node): TargetNodeType
  }

  trait ElemFactory {

    type TargetElemType <: BackingNodes.Elem

    def from(elem: BackingNodes.Elem): TargetElemType

    def from(docUriOption: Option[URI], elem: ScopedNodes.Elem): TargetElemType
  }
}
