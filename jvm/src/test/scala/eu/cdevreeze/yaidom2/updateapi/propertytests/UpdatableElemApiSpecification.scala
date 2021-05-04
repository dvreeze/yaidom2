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

package eu.cdevreeze.yaidom2.updateapi.propertytests

import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.queryapi.Nodes
import eu.cdevreeze.yaidom2.queryapi.internal.ElemWithNavigationPath
import eu.cdevreeze.yaidom2.updateapi.internal.AbstractUpdatableElem
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

trait UpdatableElemApiSpecification[N, E <: AbstractUpdatableElem.Aux[N, E]] extends UpdatableElemApiSpecificationDataProvider[N, E] {
  self: Properties =>

  protected def updateElem(e: E): E

  protected def updateElemToNodeSeq(e: E): Seq[N]

  private def updateElem(e: E, navigationPath: Seq[Int]): E = updateElem(e)

  private def updateElem(e: E, step: Int): E = updateElem(e)

  private def updateElemToNodeSeq(e: E, navigationPath: Seq[Int]): Seq[N] = updateElemToNodeSeq(e)

  private def updateElemToNodeSeq(e: E, step: Int): Seq[N] = updateElemToNodeSeq(e)

  // Correspondences between TransformableElemApi and UpdatableElemApi methods

  property("transformChildElems-as-update") = forAll { (elem: E) =>
    toResolvedElem(
      elem.transformChildElems(updateElem)) == {
      val steps: Set[Int] = ElemWithNavigationPath(elem).findAllChildElems.map(_.navigationPath.head).toSet

      toResolvedElem(
        elem.updateChildElems(steps)(updateElem))
    }
  }

  property("transformChildElemsToNodeSeq-as-update") = forAll { (elem: E) =>
    toResolvedElem(
      elem.transformChildElemsToNodeSeq(updateElemToNodeSeq)) == {
      val steps: Set[Int] = ElemWithNavigationPath(elem).findAllChildElems.map(_.navigationPath.head).toSet

      toResolvedElem(
        elem.updateChildElemsWithNodeSeq(steps)(updateElemToNodeSeq))
    }
  }

  property("transformDescendantElemsOrSelf-as-update") = forAll { (elem: E) =>
    toResolvedElem(
      elem.transformDescendantElemsOrSelf(updateElem)) == {
      val paths: Set[Seq[Int]] = ElemWithNavigationPath(elem).findAllDescendantElemsOrSelf.map(_.navigationPath).toSet

      toResolvedElem(
        elem.updateDescendantElemsOrSelf(paths)(updateElem))
    }
  }

  property("transformDescendantElemsOrSelfToNodeSeq-as-update") = forAll { (elem: E) =>
    toResolvedNodes(
      elem.transformDescendantElemsOrSelfToNodeSeq(updateElemToNodeSeq)) == {
      val paths: Set[Seq[Int]] = ElemWithNavigationPath(elem).findAllDescendantElemsOrSelf.map(_.navigationPath).toSet

      val descendantResult = elem.updateDescendantElemsWithNodeSeq(paths)(updateElemToNodeSeq)
      val result: Seq[N] =
        if (paths.contains(Nil)) updateElemToNodeSeq(descendantResult) else Seq(descendantResult.asInstanceOf[N])

      toResolvedNodes(result)
    }
  }

  property("transformDescendantElems-as-update") = forAll { (elem: E) =>
    toResolvedElem(
      elem.transformDescendantElems(updateElem)) == {
      val paths: Set[Seq[Int]] = ElemWithNavigationPath(elem).findAllDescendantElems.map(_.navigationPath).toSet

      toResolvedElem(
        elem.updateDescendantElemsOrSelf(paths)(updateElem))
    }
  }

  property("transformDescendantElemsToNodeSeq-as-update") = forAll { (elem: E) =>
    toResolvedElem(
      elem.transformDescendantElemsToNodeSeq(updateElemToNodeSeq)) == {
      val paths: Set[Seq[Int]] = ElemWithNavigationPath(elem).findAllDescendantElems.map(_.navigationPath).toSet

      val descendantResult = elem.updateDescendantElemsWithNodeSeq(paths)(updateElemToNodeSeq)
      toResolvedElem(descendantResult)
    }
  }

  // TransformableElemApi methods in terms of other ones

  property("transformDescendantElems-as-other-transform") = forAll { (elem: E) =>
    toResolvedElem(elem.transformDescendantElems(updateElem)) == {
      val result = elem.transformDescendantElemsOrSelf(_.transformChildElems(updateElem))
      toResolvedElem(result)
    }
  }

  property("transformDescendantElemsToNodeSeq-as-other-transform") = forAll { (elem: E) =>
    toResolvedElem(elem.transformDescendantElemsToNodeSeq(updateElemToNodeSeq)) == {
      val result = elem.transformDescendantElemsOrSelf(_.transformChildElemsToNodeSeq(updateElemToNodeSeq))
      toResolvedElem(result)
    }
  }

  // UpdatableElemApi methods in terms of other ones

  property("updateDescendantElemsOrSelf-as-single-path-updates-combined") = forAll { (elem: E) =>
    val somePaths: Seq[Seq[Int]] = ElemWithNavigationPath(elem).findAllDescendantElems.map(_.navigationPath).take(40)

    toResolvedElem(elem.updateDescendantElemsOrSelf(somePaths.toSet)(updateElem)) == {
      val result = somePaths.reverse.foldLeft(elem) { (accElem, path) =>
        accElem.updateDescendantElemOrSelf(path)(updateElem)
      }
      toResolvedElem(result)
    }
  }

  property("updateDescendantElemsWithNodeSeq-as-single-path-updates-combined") = forAll { (elem: E) =>
    val somePaths: Seq[Seq[Int]] = ElemWithNavigationPath(elem).findAllDescendantElems.map(_.navigationPath).take(40)

    toResolvedElem(elem.updateDescendantElemsWithNodeSeq(somePaths.toSet)(updateElemToNodeSeq)) == {
      val result = somePaths.reverse.foldLeft(elem) { (accElem, path) =>
        accElem.updateDescendantElemWithNodeSeq(path)(updateElemToNodeSeq)
      }
      toResolvedElem(result)
    }
  }

  private def toResolvedElem(e: E): resolved.Elem = {
    resolved.Elem.from(e)
  }

  private def toResolvedNodes(nodes: Seq[N]): Seq[resolved.Node] = {
    nodes.flatMap {
      case e: Nodes.Elem => Some(toResolvedElem(e.asInstanceOf[E]))
      case t: Nodes.Text => Some(resolved.Text(t.text))
      case _ => None
    }
  }
}
