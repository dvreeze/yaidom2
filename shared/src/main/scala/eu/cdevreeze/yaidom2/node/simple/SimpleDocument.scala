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

package eu.cdevreeze.yaidom2.node.simple

import eu.cdevreeze.yaidom2.queryapi.oo.DocumentApi

/**
 * Document holding a SimpleNodes.Elem.
 *
 * @author Chris de Vreeze
 */
final case class SimpleDocument(children: Seq[SimpleNodes.CanBeDocumentChild]) extends DocumentApi {
  require(
    children.collect { case e: SimpleNodes.Elem => e }.size == 1,
    s"A document must have precisely 1 document element but found ${children.collect { case e: SimpleNodes.Elem => e }.size} ones")

  type CanBeDocumentChildType = SimpleNodes.CanBeDocumentChild

  type ElemType = SimpleNodes.Elem

  def documentElement: ElemType = children.collectFirst { case e: SimpleNodes.Elem => e }.get
}

object SimpleDocument {

  def apply(documentElement: SimpleNodes.Elem): SimpleDocument = {
    apply(Seq(documentElement))
  }
}
