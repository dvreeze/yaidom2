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

import eu.cdevreeze.yaidom2.core.Scope

/**
 * This package contains "simple" element representations that do not know about their ancestry and document URI.
 *
 * @author Chris de Vreeze
 */
package object simple {

  type Document = SimpleDocument

  type Node = SimpleNodes.Node

  type CanBeDocumentChild = SimpleNodes.CanBeDocumentChild

  type Elem = SimpleNodes.Elem

  type Text = SimpleNodes.Text

  type Comment = SimpleNodes.Comment

  type ProcessingInstruction = SimpleNodes.ProcessingInstruction

  val Document = SimpleDocument

  val Node = SimpleNodes.Node

  val CanBeDocumentChild = SimpleNodes.CanBeDocumentChild

  val Elem = SimpleNodes.Elem

  val Text = SimpleNodes.Text

  val Comment = SimpleNodes.Comment

  val ProcessingInstruction = SimpleNodes.ProcessingInstruction

  private[yaidom2] def strictElemCreator(scope: Scope): SimpleElemCreator = SimpleElemCreator.strict(scope)

  private[yaidom2] def lenientElemCreator(scope: Scope): SimpleElemCreator = SimpleElemCreator.lenient(scope)
}
