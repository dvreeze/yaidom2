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

package eu.cdevreeze.yaidom2.utils.namespaces

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.BackingElemApi

/**
 * Utility to find ENames used in an XML document.
 *
 * @author Chris de Vreeze
 */
object ENameFinder {

  /**
   * Finds all ENames in the given element tree, which are element names and attribute names, as well as ENames found
   * in element text and attribute values, according to the passed DocumentENameExtractor.
   */
  def findAllENames(elem: BackingElemApi, documentENameExtractor: DocumentENameExtractor): Set[EName] = {
    elem.findAllDescendantElemsOrSelf.flatMap(e => findAllENamesInElementItself(e, documentENameExtractor)).toSet
  }

  /**
   * Returns `findAllENames(elem, documentENameExtractor).flatMap(_.namespaceUriOption)`.
   */
  def findAllNamespaces(elem: BackingElemApi, documentENameExtractor: DocumentENameExtractor): Set[String] = {
    findAllENames(elem, documentENameExtractor).flatMap(_.namespaceUriOption)
  }

  /**
   * Like `findAllElems`, but ignoring descendant elements.
   */
  def findAllENamesInElementItself(elem: BackingElemApi, documentENameExtractor: DocumentENameExtractor): Set[EName] = {
    val enamesInElemText: Set[EName] =
      documentENameExtractor.findElemTextENameExtractor(elem).map(_.extractENames(elem.scope, elem.text)).getOrElse(Set.empty)

    val enamesInAttrValues: Map[EName, Set[EName]] = elem.attributes.keys.map { attrName =>
      val enames = documentENameExtractor.findAttributeValueENameExtractor(elem, attrName)
        .map(_.extractENames(elem.scope.withoutDefaultNamespace, elem.attr(attrName))).getOrElse(Set.empty)

      attrName -> enames
    }.toMap

    Set(elem.name).concat(elem.attributes.keySet).concat(enamesInElemText).concat(enamesInAttrValues.values.flatten.toSet)
  }

  /**
   * Returns `findAllENamesInElementItself(elem, documentENameExtractor).flatMap(_.namespaceUriOption)`.
   */
  def findAllNamespacesInElementItself(elem: BackingElemApi, documentENameExtractor: DocumentENameExtractor): Set[String] = {
    findAllENamesInElementItself(elem, documentENameExtractor).flatMap(_.namespaceUriOption)
  }
}
