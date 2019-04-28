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
 * This package contains "indexed" element representations, where each element is found by following a "navigation
 * path" on an underlying "simple" element tree.
 *
 * @author Chris de Vreeze
 */
package object indexed {

  type Document = IndexedDocument

  type Node = IndexedNodes.Node

  type CanBeDocumentChild = IndexedNodes.CanBeDocumentChild

  type Elem = IndexedNodes.Elem

  type Text = IndexedNodes.Text

  type Comment = IndexedNodes.Comment

  type ProcessingInstruction = IndexedNodes.ProcessingInstruction

  val Document = IndexedDocument

  val Elem = IndexedNodes.Elem

  val Text = IndexedNodes.Text

  val Comment = IndexedNodes.Comment

  val ProcessingInstruction = IndexedNodes.ProcessingInstruction
}
