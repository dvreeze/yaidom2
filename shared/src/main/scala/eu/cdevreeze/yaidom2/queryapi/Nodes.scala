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

package eu.cdevreeze.yaidom2.queryapi

/**
 * Abstract node (marker) trait hierarchy. It offers a common minimal API for different kinds of nodes. It also shows
 * what yaidom2 typically considers to be nodes, and what it does not consider to be nodes. For example, documents
 * are not nodes in yaidom2, so it is thus impossible to create documents as element children. Moreover, attributes
 * are typically not nodes in yaidom2, although custom element implementations may think otherwise.
 *
 * Like in the XPath Data Model, entity references are not modelled as nodes. Unlike the XDM, yaidom2 does not consider
 * namespace nodes to exist as nodes. Depending on the abstraction level, element nodes can have in-scope namespaces, though,
 * but not as nodes.
 *
 * @author Chris de Vreeze
 */
object Nodes {

  /**
   * Arbitrary node
   */
  trait Node {

    def nodeKind: NodeKind
  }

  /**
   * Potential document child, so an element, processing instruction or comment
   */
  trait CanBeDocumentChild extends Node

  /**
   * Arbitrary element node
   */
  trait Elem extends CanBeDocumentChild {

    final def nodeKind: NodeKind = ElementKind
  }

  /**
   * Arbitrary text node
   */
  trait Text extends Node {

    final def nodeKind: NodeKind = TextKind

    def text: String
  }

  /**
   * Arbitrary comment node
   */
  trait Comment extends CanBeDocumentChild {

    final def nodeKind: NodeKind = CommentKind

    def text: String
  }

  /**
   * Arbitrary processing instruction node
   */
  trait ProcessingInstruction extends CanBeDocumentChild {

    final def nodeKind: NodeKind = ProcessingInstructionKind

    def target: String

    def data: String
  }

  /** Node kind, which can be used for cheap pattern matching on kinds of nodes */
  sealed trait NodeKind
  case object ElementKind extends NodeKind
  case object TextKind extends NodeKind
  case object CommentKind extends NodeKind
  case object ProcessingInstructionKind extends NodeKind
}
