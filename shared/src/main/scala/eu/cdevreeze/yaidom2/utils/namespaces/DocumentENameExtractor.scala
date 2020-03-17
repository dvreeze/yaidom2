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
 * Strategy trait for finding TextENameExtractors in a document. Hence implementations of this trait can retrieve all ENames and
 * therefore all namespaces used in a document.
 *
 * ENames occur in an element tree as element names and attribute names, but they can also occur in element text or attribute
 * values (as QNames to be resolved against the Scope as ENames). The latter 2 typically follow from an XML Schema. This schema
 * knowledge about used ENames can be coded as a DocumentENameExtractor. That is, for specific elements the DocumentENameExtractor
 * can return a TextENameExtractor to extract ENames from the element text. Moreover, for specific elements and specific attributes
 * in those elements the DocumentENameExtractor can return a TextENameExtractor to extract ENames from the attribute value.
 *
 * A utility that finds all ENames used in an XML document could use a DocumentENameExtractor to help it in doing so.
 * Element names and attribute names would be found effortlessly, whereas ENames in element text and attribute values would
 * be found using the DocumentENameExtractor.
 *
 * It may be a bit restrictive to offer this API only for "backing elements", but sometimes the ancestry of the element
 * matters in order to "interpret" the element. If the ancestry is irrelevant, just ignore it and create a "backing element"
 * from the element without any ancestry. If the parent element is relevant, consider adding one via an "element creation DSL"
 * (and then query for the correct child element, which then has a parent element).
 *
 * @author Chris de Vreeze
 */
trait DocumentENameExtractor {

  /**
   * Returns an optional TextENameExtractor for interpreting element text of the parameter element.
   */
  def findElemTextENameExtractor(elem: BackingElemApi): Option[TextENameExtractor]

  /**
   * Returns an optional TextENameExtractor for interpreting the attribute value of the parameter element and parameter attribute name.
   */
  def findAttributeValueENameExtractor(elemApi: BackingElemApi, attrName: EName): Option[TextENameExtractor]
}

object DocumentENameExtractor {

  /**
   * DocumentENameExtractor that never returns any TextENameExtractor for element text or attribute values.
   */
  val NoOp: DocumentENameExtractor = new DocumentENameExtractor {

    override def findElemTextENameExtractor(elem: BackingElemApi): Option[TextENameExtractor] = None

    override def findAttributeValueENameExtractor(elemApi: BackingElemApi, attrName: EName): Option[TextENameExtractor] = None
  }
}
