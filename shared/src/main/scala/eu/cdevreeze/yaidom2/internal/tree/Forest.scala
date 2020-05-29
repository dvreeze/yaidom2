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

package eu.cdevreeze.yaidom2.internal.tree

/**
 * Generic forest monad. The entire forest is eagerly evaluated.
 *
 * @author Chris de Vreeze
 */
final case class Forest[A](trees: Seq[Tree[A]]) {

  /**
   * Returns the equivalent of `Forest(trees.map(_.map(f)))`, or `flatMap(f.andThen(Forest.pure))`.
   */
  def map[B](f: A => B): Forest[B] = {
    Forest(trees.map(_.map(f)))
  }

  def flatMap[B](f: A => Forest[B]): Forest[B] = {
    val treeSeq: Seq[Tree[B]] = trees.flatMap { tr =>
      val forest: Forest[B] = f(tr.nodeContent)
      // Recursive calls, but not tail-recursive
      forest.trees.map { t =>
        t.copy(children = Forest(tr.children).flatMap(f).trees)
      }
    }
    Forest(treeSeq)
  }
}

object Forest {

  def pure[A](value: A): Forest[A] = Forest(Seq(Tree.pure(value)))
}
