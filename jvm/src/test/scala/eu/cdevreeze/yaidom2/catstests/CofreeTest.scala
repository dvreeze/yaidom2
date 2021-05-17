/*
 * Copyright 2019-2021 Chris de Vreeze
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

package eu.cdevreeze.yaidom2.catstests

import java.io.File
import java.net.URI

import cats._
import cats.free.Cofree
import cats.instances.seq._
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.saxon.SaxonDocument
import eu.cdevreeze.yaidom2.node.simple
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite
import scala.util.chaining._

import eu.cdevreeze.yaidom2.core.EName

class CofreeTest extends AnyFunSuite {

  import CofreeTest._

  private val processor = new Processor(false)

  test("unfoldTree") {
    val docUri: URI = classOf[EqTest].getResource("/test-xml/airportsGermany.xml").toURI
    val doc: SaxonDocument = SaxonDocument(processor.newDocumentBuilder().build(new File(docUri)))

    // Unfold is recursive construction
    val saxonTree: SaxonTree = Cofree.unfold[Seq, saxon.Node](doc.documentElement)(n => findAllChildren(n))

    assertResult(doc.documentElement) {
      saxonTree.head
    }

    val resolvedTreeEval: Eval[resolved.Node] = Cofree.cata[Seq, saxon.Node, resolved.Node](saxonTree) {
      (node: saxon.Node, mappedChildren: Seq[resolved.Node]) =>
        val mappedNode: resolved.Node = resolved.Node.from(node).pipe {
          case e: resolved.Elem => e.withChildren(mappedChildren)
          case n                => n
        }
        Eval.now(mappedNode)
    }

    assertResult(resolved.Node.from(doc.documentElement)) {
      resolvedTreeEval.value
    }
  }

  test("unfoldNodeTree") {
    // Now using NodeTree instead of generic Tree. This is not an improvement, as can be seen below.

    val docUri: URI = classOf[EqTest].getResource("/test-xml/airportsGermany.xml").toURI
    val doc: SaxonDocument = SaxonDocument(processor.newDocumentBuilder().build(new File(docUri)))

    // Unfold is recursive construction
    val saxonNodeTree: SaxonNodeTree =
      Cofree.unfold[Seq, Node[saxon.Node]](Node[saxon.Node](doc.documentElement)) { n =>
        findAllChildren(n)
      }

    assertResult(doc.documentElement) {
      saxonNodeTree.head.underlyingNode
    }

    val resolvedTreeEval: Eval[resolved.Node] = Cofree.cata[Seq, Node[saxon.Node], resolved.Node](saxonNodeTree) {
      (node: Node[saxon.Node], mappedChildren: Seq[resolved.Node]) =>
        val mappedNode: resolved.Node = resolved.Node.from(node.underlyingNode).pipe {
          case e: resolved.Elem => e.withChildren(mappedChildren)
          case n                => n
        }
        Eval.now(mappedNode)
    }

    assertResult(resolved.Node.from(doc.documentElement)) {
      resolvedTreeEval.value
    }
  }

  test("addAncestry") {
    // Adding the reverse-ancestry-or-self without defining a separate custom recursive node type for that.

    val docUri: URI = classOf[EqTest].getResource("/test-xml/airportsGermany.xml").toURI
    val doc: SaxonDocument = SaxonDocument(processor.newDocumentBuilder().build(new File(docUri)))

    // Unfold is recursive construction
    val resolvedDocElem: resolved.Elem = resolved.Elem.from(doc.documentElement)

    val resolvedTreeWithAncestry: Tree[ResolvedNodeWithAncestry] =
      Cofree.unfold[Seq, ResolvedNodeWithAncestry](ResolvedNodeWithAncestry(resolvedDocElem, Nil)) { n =>
        findAllChildren(n.node).map(ch => ResolvedNodeWithAncestry(ch, n.node.asInstanceOf[resolved.Elem].name :: n.ancestorNames.toList))
      }

    assertResult(doc.documentElement.findAllDescendantElemsOrSelf.map(_.findAllAncestorElemsOrSelf.map(_.name)).toSet) {
      Tree.findAllDescendantsOrSelf(resolvedTreeWithAncestry).map(_.head.ancestorOrSelfNames).toSet
    }

    assertResult(doc.documentElement.findAllDescendantElemsOrSelf.map(_.findAllAncestorElems.map(_.name)).toSet) {
      Tree
        .filterDescendantsOrSelf(
          resolvedTreeWithAncestry,
          (tree: Tree[ResolvedNodeWithAncestry]) => tree.head.node.isInstanceOf[resolved.Elem])
        .map(_.head.ancestorNames)
        .toSet
    }
  }
}

object CofreeTest {

  // Note below that indeed Seq is a functor (i.e. there is a functor type class instance for Seq).
  // Recall: case class Cofree[F[_], A](head: A, tail: Eval[F[Cofree[F, A]]])

  type Tree[A] = Cofree[Seq, A] // Think: case class Tree[A](head: A, tail: Eval[Seq[Tree[A]]])

  object Tree {

    def findAllChildren[A](tree: Tree[A]): Seq[Tree[A]] = tree.tailForced

    def filterChildren[A](tree: Tree[A], p: Tree[A] => Boolean): Seq[Tree[A]] = findAllChildren(tree).filter(p)

    def findAllDescendantsOrSelf[A](tree: Tree[A]): Seq[Tree[A]] = {
      filterDescendantsOrSelf(tree, _ => true)
    }

    def filterDescendantsOrSelf[A](tree: Tree[A], p: Tree[A] => Boolean): Seq[Tree[A]] = {
      // Recursive calls
      findAllChildren(tree)
        .flatMap(ch => filterDescendantsOrSelf(ch, p))
        .prependedAll(Option(tree).filter(p).toSeq)
    }

    def findAllDescendants[A](tree: Tree[A]): Seq[Tree[A]] = {
      filterDescendants(tree, _ => true)
    }

    def filterDescendants[A](tree: Tree[A], p: Tree[A] => Boolean): Seq[Tree[A]] = {
      findAllChildren(tree).flatMap(ch => filterDescendantsOrSelf(ch, p))
    }
  }

  type Forest[A] = Seq[Tree[A]]

  type ResolvedTree = Tree[resolved.Node]

  type SimpleTree = Tree[simple.Node]

  type SaxonTree = Tree[saxon.Node]

  final case class ResolvedNodeWithAncestry(node: resolved.Node, ancestorNames: Seq[EName]) {

    def ancestorOrSelfNames: Seq[EName] = {
      val nameOption: Option[EName] = node match {
        case e: resolved.Elem => Some(e.name)
        case _                => None
      }
      ancestorNames.prependedAll(nameOption.toSeq)
    }
  }

  // Specific functor, which conceptually hardly differs from Seq.
  // Think of the Node class as a recursive type, where the recursion has been replaced by a type parameter,
  // which typically is or contains type Node itself. Being a type parameter gives its use in Cofree
  // the flexibility to change "tree node types". Still, from an OO perspective, these custom functors
  // feel a bit strange to me.

  final case class Node[A](underlyingNode: ClarkNodes.Node, children: Seq[A])

  object Node {

    def apply[A <: ClarkNodes.Node](underlyingNode: A): Node[A] =
      apply(underlyingNode, findAllChildren(underlyingNode))
  }

  // Not needed?? What about implicit Traverse?
  implicit val nodeFunctor: Functor[Node] = new Functor[Node] {
    def map[A, B](fa: Node[A])(f: A => B): Node[B] = {
      Node[B](fa.underlyingNode, fa.children.map(f))
    }
  }

  type NodeTree[A] = Cofree[Seq, Node[A]]

  type NodeForest[A] = Seq[NodeTree[A]]

  type SimpleNodeTree = NodeTree[simple.Node]

  type SaxonNodeTree = NodeTree[saxon.Node]

  def findAllChildren[A <: ClarkNodes.Node](node: A): Seq[A] = {
    node match {
      case e: ClarkNodes.Elem => e.children.asInstanceOf[Seq[A]]
      case _                  => Seq.empty
    }
  }

  def findAllChildren[A <: ClarkNodes.Node](node: Node[A]): Seq[Node[A]] = {
    node.underlyingNode match {
      case e: ClarkNodes.Elem => e.children.asInstanceOf[Seq[A]].map(ch => Node[A](ch))
      case _                  => Seq.empty
    }
  }
}
