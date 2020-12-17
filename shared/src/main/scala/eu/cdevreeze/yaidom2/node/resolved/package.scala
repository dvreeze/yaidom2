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
 * This package contains "minimal" element representations that can be compared for (some notion of "value") equality.
 * This notion of equality is simple to understand, but "naive". The user of the API must take control over what
 * is compared for equality.
 *
 * @author Chris de Vreeze
 */
package object resolved {

  type Document = ResolvedDocument

  type Node = ResolvedNodes.Node

  type CanBeDocumentChild = ResolvedNodes.CanBeDocumentChild

  type Elem = ResolvedNodes.Elem

  type Text = ResolvedNodes.Text

  val Document = ResolvedDocument

  val Node = ResolvedNodes.Node

  val Elem = ResolvedNodes.Elem

  val Text = ResolvedNodes.Text

  // Wrappers

  type ElemInKnownScope = ResolvedNodes.ElemInKnownScope

  val ElemInKnownScope = ResolvedNodes.ElemInKnownScope
}
