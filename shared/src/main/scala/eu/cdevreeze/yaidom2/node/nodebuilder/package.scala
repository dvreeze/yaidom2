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

package eu.cdevreeze.yaidom2.node

/**
 * This package contains an element builder DSL, offering the ScopedNodes.Elem query API. The DSL is as friendly
 * to create as resolved elements, but it retains a Scope. The ease of use comes at the price that this Scope must be
 * invertible and contain no default namespace. Also, all elements must have the same Scope, or, at least each descendant
 * element must have at least the same Scope or a super-set.
 *
 * @author Chris de Vreeze
 */
package object nodebuilder {

  type Document = NodeBuilders.Document

  type Node = NodeBuilders.Node

  type CanBeDocumentChild = NodeBuilders.CanBeDocumentChild

  type Elem = NodeBuilders.Elem

  type Text = NodeBuilders.Text

  val Document = NodeBuilders.Document

  val Node = NodeBuilders.Node

  val Elem = NodeBuilders.Elem
}
