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

package eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.internal

import eu.cdevreeze.yaidom2.node.indexed
import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.queryapi.oo.BackingNodes
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.xpointer.XPointer

object BackingNodesUtil {

  def findElem(elem: BackingNodes.Elem, xpointer: XPointer): Option[BackingNodes.Elem] = {
    elem match {
      case elem: saxon.Elem => xpointer.findElem(elem)
      case elem: indexed.Elem => xpointer.findElem(elem)
      case elem => sys.error(s"Unknown element type: ${elem.getClass}")
    }
  }
}
