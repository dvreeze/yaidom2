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
 * Generic tree functor. The entire tree is eagerly evaluated.
 *
 * @author Chris de Vreeze
 */
final case class Tree[A](nodeContent: A, children: Seq[Tree[A]]) {

  // A Tree[A] has no useful flatMap method, but its map method does indeed make sense

  def map[B](f: A => B): Tree[B] = {
    // Recursive calls, but not tail-recursive
    Tree.pure(f(nodeContent)).copy(children = this.children.map(_.map(f)))
  }
}

object Tree {

  def leaf[A](value: A): Tree[A] = tree(value, Seq.empty)

  def tree[A](value: A, children: Seq[Tree[A]]): Tree[A] = Tree(value, children)

  def pure[A](value: A): Tree[A] = leaf(value)
}
