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

import cats.Eq
import cats.implicits._
import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.saxon.SaxonDocument
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import net.sf.saxon.s9api.Processor
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.net.URI

class EqTest extends AnyFunSuite {

  // What did I learn from this exercise?
  // Type class instance selection can be quite sensitive, in this case for the Cats Eq type class.
  // First of all, take control over the compile-time parameter type, such as Map instead of ListMap (so add toMap calls).
  // That is, have the type class instance for the appropriate super-type (for covariant types) selected by the compiler.
  // Also mind the possibility that multiple type class instances for the same parameter type exist, leading to a compilation error.
  // Another thing is that type class OO syntax (in this case the triple-equals operator) may clash between
  // Cats and Scalactic. Hence the workaround below, using named method eqv instead.

  // The approach of handling different XML equality semantics by introducing specific equality-wrappers for
  // elements and arbitrary nodes, and introducing Eq type class instances for them, works out nicely.
  // There is no default XML equality semantics, nor has there to be any. We can still leverage the
  // Cats Eq type class, but make the XML equality semantics explicit by the use of specific wrapper instances
  // that are compared for equality by their respective Cats Eq type class instances.

  import EqTest._

  private val processor = new Processor(false)

  test("conversionResultEquality") {
    val docUri: URI = classOf[EqTest].getResource("/test-xml/airportsGermany.xml").toURI
    val doc: SaxonDocument = SaxonDocument(processor.newDocumentBuilder().build(new File(docUri)))

    val elem1: ClarkNodes.Elem = doc.documentElement
    val elem2: ClarkNodes.Elem = resolved.Elem.from(elem1)

    assertResult(true) {
      elem1.clarkElem eqv elem2.clarkElem // circumventing conflict with "===" of scalactic
    }

    assertResult(true) {
      elem1.clarkNode eqv elem2.clarkNode // circumventing conflict with "===" of scalactic
    }
  }
}

object EqTest {

  // Testing specifically for "Clark element equality" or "Clark node equality"

  final case class ClarkNodeEqWrapper(underlying: ClarkNodes.Node)

  implicit class ToClarkNodeEqWrapper(val underlying: ClarkNodes.Node) extends AnyVal {

    def clarkNode: ClarkNodeEqWrapper = ClarkNodeEqWrapper(underlying)
  }

  final case class ClarkElemEqWrapper(underlying: ClarkNodes.Elem)

  implicit class ToClarkElemEqWrapper(val underlying: ClarkNodes.Elem) extends AnyVal {

    def clarkElem: ClarkElemEqWrapper = ClarkElemEqWrapper(underlying)
  }

  implicit val enameEq: Eq[EName] = Eq.instance[EName] { (name1: EName, name2: EName) =>
    name1.namespaceUriOption === name2.namespaceUriOption && name1.localPart === name2.localPart
  }

  implicit val clarkNodeEq: Eq[ClarkNodeEqWrapper] = Eq.instance[ClarkNodeEqWrapper] {
    (node1: ClarkNodeEqWrapper, node2: ClarkNodeEqWrapper) =>
      (node1.underlying, node2.underlying) match {
        case (txt1: ClarkNodes.Text, txt2: ClarkNodes.Text) =>
          txt1.text === txt2.text
        case (com1: ClarkNodes.Comment, com2: ClarkNodes.Comment) =>
          com1.text === com2.text
        case (pi1: ClarkNodes.ProcessingInstruction, pi2: ClarkNodes.ProcessingInstruction) =>
          pi1.target === pi2.target && pi1.data === pi2.data
        case (elem1: ClarkNodes.Elem, elem2: ClarkNodes.Elem) =>
          elem1.name === elem2.name &&
            elem1.attributes.toMap === elem2.attributes.toMap &&
            elem1.children.map(_.clarkNode) === elem2.children.map(_.clarkNode)
        case _ => false
      }
  }

  implicit val clarkElemEq: Eq[ClarkElemEqWrapper] = Eq.instance[ClarkElemEqWrapper] {
    (elem1: ClarkElemEqWrapper, elem2: ClarkElemEqWrapper) =>
      elem1.underlying.name === elem2.underlying.name &&
      elem1.underlying.attributes.toMap === elem2.underlying.attributes.toMap &&
      eq(elemOrTextChildren(elem1.underlying).map(toEqWrapper), elemOrTextChildren(elem2.underlying).map(toEqWrapper))
  }

  private def elemOrTextChildren(elem: ClarkNodes.Elem): Seq[ClarkNodes.Node] = {
    elem.children.collect {
      case e: ClarkNodes.Elem => e
      case t: ClarkNodes.Text => t
    }
  }

  private def toEqWrapper(node: ClarkNodes.Node): AnyRef = {
    node match {
      case e: ClarkNodes.Elem => e.clarkElem
      case n                  => n.clarkNode
    }
  }

  private def eq(nodes1: Seq[_], nodes2: Seq[_]): Boolean = {
    nodes1.size === nodes2.size && {
      nodes1.zip(nodes2).forall {
        case (node1, node2) =>
          (node1, node2) match {
            case (e1: ClarkElemEqWrapper, e2: ClarkElemEqWrapper) => e1 === e2
            case (n1: ClarkNodeEqWrapper, n2: ClarkNodeEqWrapper) => n1 === n2
            case _                                                => false
          }
      }
    }
  }
}
