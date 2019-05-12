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

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.node.resolved
import eu.cdevreeze.yaidom2.node.simple

final class SimpleElemFactory(val knownScope: Scope) {

  import SimpleElemFactory.ScopedQName

  val knownScopeWithoutDefaultNamespace = knownScope.withoutDefaultNamespace

  def fromResolvedElem(elem: resolved.Elem, parentScope: Scope): simple.Elem = {
    var scope = parentScope

    val scopedQName = convertToQName(elem.name, parentScope)
    val qname: QName = scopedQName.qname

    scope = scope ++ scopedQName.scope

    val attributesByQName: SeqMap[QName, String] = elem.attributes.map { case (attrName, attrValue) =>
      val scopedAttrQName = convertToQName(attrName, scope.withoutDefaultNamespace)
        val attrQName: QName = scopedAttrQName.qname

        scope = scope ++ scopedAttrQName.scope

        attrQName -> attrValue
    }

    val children: Seq[simple.Node] = elem.children.collect {
      case childElm: resolved.Elem =>
        // Recursive call
        fromResolvedElem(childElm, scope)
      case childText: resolved.Text =>
        simple.Text(childText.text, false)
    }

    new simple.Elem(qname, attributesByQName, scope, children.to(ArraySeq))
  }

  def convertToQName(name: EName, scope: Scope): ScopedQName = {
    name match {
      case EName(None, localName) if scope.defaultNamespaceOption.isEmpty =>
        ScopedQName(QName.fromLocalName(localName), scope)
      case EName(None, localName) =>
        sys.error(s"Could not extract QName from EName $name due to missing default namespace")
      case EName(Some("http://www.w3.org/XML/1998/namespace"), localName) =>
        ScopedQName(QName("xml", localName), scope)
      case EName(Some(ns), localName) =>
        val nextScope: Scope =
          if (scope.prefixesForNamespace(ns).nonEmpty) {
            scope
          } else {
            require(
              knownScopeWithoutDefaultNamespace.prefixesForNamespace(ns).nonEmpty,
              s"Unknown prefix for QName resolving to EName $name")

            // TODO Throw exception if conflicting prefix

            scope ++ Scope.from(knownScopeWithoutDefaultNamespace.prefixesForNamespace(ns).head -> ns)
          }

        val rawPrefix = scope.prefixesForNamespace(ns).head
        val prefixOption = if (rawPrefix.isEmpty) None else Some(rawPrefix)

        ScopedQName(QName(prefixOption, localName), nextScope)
    }
  }
}

object SimpleElemFactory {

  case class ScopedQName(qname: QName, scope: Scope)
}
