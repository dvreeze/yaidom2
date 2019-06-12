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

package eu.cdevreeze.yaidom2.updateapi.internal

import scala.collection.immutable.SortedMap

import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.Nodes
import eu.cdevreeze.yaidom2.queryapi.internal.ElemWithNavigationPath
import eu.cdevreeze.yaidom2.updateapi.UpdatableElemApi

/**
 * Abstract partially implemented UpdatableElemApi, for re-usable (but overridable) partial element implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not a stable as the purely abstract API.
 *
 * @author Chris de Vreeze
 */
trait AbstractUpdatableElem extends AbstractTransformableElem with UpdatableElemApi {

  type ThisNode >: ThisElem <: ClarkNodes.Node

  type ThisElem <: AbstractUpdatableElem.Aux[ThisNode, ThisElem]

  def updateChildElem(navigationStep: Int)(f: ThisElem => ThisElem): ThisElem = {
    updateChildElems(Set(navigationStep)) { (elm, _) => f(elm) }
  }

  def updateChildElem(navigationStep: Int, newElem: ThisElem): ThisElem = {
    updateChildElem(navigationStep)(_ => newElem)
  }

  def updateChildElemWithNodeSeq(navigationStep: Int)(f: ThisElem => Seq[ThisNode]): ThisElem = {
    updateChildElemsWithNodeSeq(Set(navigationStep)) { (elm, _) => f(elm) }
  }

  def updateChildElemWithNodeSeq(navigationStep: Int, newNodes: Seq[ThisNode]): ThisElem = {
    updateChildElemWithNodeSeq(navigationStep)(_ => newNodes)
  }

  def updateDescendantElemOrSelf(navigationPath: Seq[Int])(f: ThisElem => ThisElem): ThisElem = {
    updateDescendantElemsOrSelf(Set(navigationPath)) { (elm, _) => f(elm) }
  }

  def updateDescendantElemOrSelf(navigationPath: Seq[Int], newElem: ThisElem): ThisElem = {
    updateDescendantElemOrSelf(navigationPath)(_ => newElem)
  }

  def updateDescendantElemWithNodeSeq(navigationPath: Seq[Int])(f: ThisElem => Seq[ThisNode]): ThisElem = {
    updateDescendantElemsWithNodeSeq(Set(navigationPath)) { (elm, _) => f(elm) }
  }

  def updateDescendantElemWithNodeSeq(navigationPath: Seq[Int], newNodes: Seq[ThisNode]): ThisElem = {
    updateDescendantElemWithNodeSeq(navigationPath)(_ => newNodes)
  }

  def updateChildElems(navigationSteps: Set[Int])(f: (ThisElem, Int) => ThisElem): ThisElem = {
    val stepToNodeIndex: SortedMap[Int, Int] = getStepToChildNodeIndexMapReversed(navigationSteps)

    val newChildren: Seq[ThisNode] = stepToNodeIndex.foldLeft(findAllChildNodes) { case (accChildren, (step, nodeIndex)) =>
      val childElem = accChildren(nodeIndex).asInstanceOf[ThisElem]
      accChildren.updated(nodeIndex, f(childElem, step))
    }

    withChildren(newChildren)
  }

  def updateChildElemsWithNodeSeq(navigationSteps: Set[Int])(f: (ThisElem, Int) => Seq[ThisNode]): ThisElem = {
    val stepToNodeIndex: SortedMap[Int, Int] = getStepToChildNodeIndexMapReversed(navigationSteps)

    val newChildren: Seq[ThisNode] = stepToNodeIndex.foldLeft(findAllChildNodes) { case (accChildren, (step, nodeIndex)) =>
      val childElem = accChildren(nodeIndex).asInstanceOf[ThisElem]
      accChildren.patch(nodeIndex, f(childElem, step), 1)
    }

    withChildren(newChildren)
  }

  def updateDescendantElemsOrSelf(navigationPaths: Set[Seq[Int]])(f: (ThisElem, Seq[Int]) => ThisElem): ThisElem = {
    val navigationPathsByFirstStep: Map[Int, Set[Seq[Int]]] =
      navigationPaths.filter(_.nonEmpty).groupBy(_.head)

    val descendantUpdateResult: ThisElem =
      updateChildElems(navigationPathsByFirstStep.keySet) {
        case (che, step) =>
          // Recursive (but non-tail-recursive) call
          che.updateDescendantElemsOrSelf(navigationPathsByFirstStep(step).map(_.drop(1))) {
            case (e, p) =>
              f(e, p.prepended(step))
          }
      }

    if (navigationPaths.contains(Seq.empty)) f(descendantUpdateResult, Seq.empty) else descendantUpdateResult
  }

  def updateDescendantElemsWithNodeSeq(navigationPaths: Set[Seq[Int]])(f: (ThisElem, Seq[Int]) => Seq[ThisNode]): ThisElem = {
    val navigationPathsByFirstStep: Map[Int, Set[Seq[Int]]] =
      navigationPaths.filter(_.nonEmpty).groupBy(_.head)

    updateChildElemsWithNodeSeq(navigationPathsByFirstStep.keySet) {
      case (che, step) =>
        che.updateDescendantElemsOrSelfWithNodeSeq(navigationPathsByFirstStep(step).map(_.drop(1))) {
          case (e, p) =>
            f(e, p.prepended(step))
        }
    }
  }

  def updateChildElems(f: PartialFunction[(ThisElem, Int), ThisElem]): ThisElem = {
    val editsByStep: Map[Int, ThisElem] =
      findAllChildElemsWithSteps.collect { case childElemWithStep if f.isDefinedAt(childElemWithStep) =>
        (childElemWithStep._2, f(childElemWithStep))
      }.toMap

    updateChildElems(editsByStep.keySet) { case (che, step) =>
      editsByStep.getOrElse(step, che)
    }
  }

  def updateChildElemsWithNodeSeq(f: PartialFunction[(ThisElem, Int), Seq[ThisNode]]): ThisElem = {
    val editsByStep: Map[Int, Seq[ThisNode]] =
      findAllChildElemsWithSteps.collect { case childElemWithStep if f.isDefinedAt(childElemWithStep) =>
        (childElemWithStep._2, f(childElemWithStep))
      }.toMap

    updateChildElemsWithNodeSeq(editsByStep.keySet) { case (che, step) =>
      editsByStep.getOrElse(step, Seq(che))
    }
  }

  def updateTopmostElemsOrSelf(f: PartialFunction[(ThisElem, Seq[Int]), ThisElem]): ThisElem = {
    var editsByNavigationPaths: Map[Seq[Int], ThisElem] = Map.empty

    ElemWithNavigationPath(self).findTopmostElemsOrSelf { elem =>
      val elemNavigationPathPair: (ThisElem, Seq[Int]) = (elem.elem, elem.navigationPath)
      val isDefined = f.isDefinedAt(elemNavigationPathPair)

      if (isDefined) {
        editsByNavigationPaths += (elem.navigationPath -> f(elemNavigationPathPair))
      }

      isDefined
    }

    updateDescendantElemsOrSelf(editsByNavigationPaths.keySet) { case (elem, navigationPath) =>
      editsByNavigationPaths.getOrElse(navigationPath, elem)
    }
  }

  def updateTopmostElemsWithNodeSeq(f: PartialFunction[(ThisElem, Seq[Int]), Seq[ThisNode]]): ThisElem = {
    var editsByNavigationPaths: Map[Seq[Int], Seq[ThisNode]] = Map.empty

    ElemWithNavigationPath(self).findTopmostElems { elem =>
      val elemNavigationPathPair: (ThisElem, Seq[Int]) = (elem.elem, elem.navigationPath)
      val isDefined = f.isDefinedAt(elemNavigationPathPair)

      if (isDefined) {
        editsByNavigationPaths += (elem.navigationPath -> f(elemNavigationPathPair))
      }

      isDefined
    }

    updateDescendantElemsWithNodeSeq(editsByNavigationPaths.keySet) { case (elem, navigationPath) =>
      editsByNavigationPaths.getOrElse(navigationPath, Seq(elem))
    }
  }

  protected[yaidom2] def self: ThisElem

  /**
   * Computes a mapping from navigation steps (child element indexes) to child node indexes, sorted in reverse document order.
   */
  protected def getStepToChildNodeIndexMapReversed(navigationSteps: Set[Int]): SortedMap[Int, Int] = {
    var result = List.empty[(Int, Int)]
    var step = 0

    findAllChildNodes.zipWithIndex.foreach { case (ch, nodeIndex) =>
      ch match {
        case che: Nodes.Elem =>
          if (navigationSteps.contains(step)) {
            result = (step, nodeIndex) :: result
          }
          step += 1
        case ch: Nodes.Node =>
      }
    }

    result.to(SortedMap)
  }

  /**
   * Returns all child element nodes coupled with their child element index.
   */
  protected def findAllChildElemsWithSteps: Seq[(ThisElem, Int)]

  /**
   * Functionally updates this element tree at the given navigation paths (relative to this element). It is equivalent
   * to calling method `updateDescendantElemOrSelfWithNodeSeq(navigationPath)(f)` repeatedly, but in reverse document order (in order not to
   * undo earlier updates).
   *
   * For efficiency it is best to pass only small sets of navigation paths.
   */
  protected def updateDescendantElemsOrSelfWithNodeSeq(navigationPaths: Set[Seq[Int]])(f: (ThisElem, Seq[Int]) => Seq[ThisNode]): Seq[ThisNode] = {
    val navigationPathsByFirstStep: Map[Int, Set[Seq[Int]]] =
      navigationPaths.filter(_.nonEmpty).groupBy(_.head)

    val descendantUpdateResult: ThisElem =
      updateChildElemsWithNodeSeq(navigationPathsByFirstStep.keySet) {
        case (che, step) =>
          // Recursive (but non-tail-recursive) call
          che.updateDescendantElemsOrSelfWithNodeSeq(navigationPathsByFirstStep(step).map(_.drop(1))) {
            case (e, p) =>
              f(e, p.prepended(step))
          }
      }

    if (navigationPaths.contains(Seq.empty)) f(descendantUpdateResult, Seq.empty) else Seq(descendantUpdateResult)
  }
}

object AbstractUpdatableElem {

  type Aux[N, E] = AbstractUpdatableElem {type ThisNode = N; type ThisElem = E}
}
