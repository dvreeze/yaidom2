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

package eu.cdevreeze.yaidom2.queryapi.fun.internal

import scala.collection.mutable

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.fun.ClarkElemFunctionsApi

/**
 * Abstract partially implemented ClarkElemFunctionsApi, for re-usable (but overridable) partial element function implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not a stable as the purely abstract API.
 *
 * @author Chris de Vreeze
 */
trait AbstractClarkElemFunctions extends ClarkElemFunctionsApi {

  protected[yaidom2] def toImmutableSeq(xs: collection.Seq[ElemType]): Seq[ElemType]

  // ElemFunctionsApi

  def findAllChildElems(elem: ElemType): Seq[ElemType] = {
    filterChildElems(elem, _ => true)
  }

  def filterDescendantElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    findAllChildElems(elem).flatMap(e => filterDescendantElemsOrSelf(e, p))
  }

  def findAllDescendantElems(elem: ElemType): Seq[ElemType] = {
    filterDescendantElems(elem, _ => true)
  }

  def findDescendantElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    findAllChildElems(elem).view.flatMap(e => findDescendantElemOrSelf(e, p)).headOption
  }

  def filterDescendantElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    val result = mutable.ArrayBuffer[ElemType]()

    def accumulate(elm: ElemType): Unit = {
      if (p(elm)) result += elm
      // Recursive calls (not tail-recursive, but the depth is typically limited)
      findAllChildElems(elm).foreach(accumulate)
    }

    accumulate(elem)
    toImmutableSeq(result)
  }

  def findAllDescendantElemsOrSelf(elem: ElemType): Seq[ElemType] = {
    filterDescendantElemsOrSelf(elem, _ => true)
  }

  def findDescendantElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType] = {
    var result: Option[ElemType] = None

    def findElem(elm: ElemType): Unit = {
      if (result.isEmpty) {
        if (p(elm)) result = Some(elm)
      }
      if (result.isEmpty) {
        // Recursive calls (not tail-recursive, but the depth is typically limited)
        findAllChildElems(elm).foreach(findElem)
      }
    }

    findElem(elem)
    result
  }

  def findTopmostElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    findAllChildElems(elem).flatMap(e => findTopmostElemsOrSelf(e, p))
  }

  def findTopmostElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType] = {
    val result = mutable.ArrayBuffer[ElemType]()

    def accumulate(elm: ElemType): Unit = {
      if (p(elm)) {
        result += elm
      } else {
        // Recursive calls (not tail-recursive, but the depth is typically limited)
        findAllChildElems(elm).foreach(accumulate)
      }
    }

    accumulate(elem)
    toImmutableSeq(result)
  }

  // ClarkElemFunctionsApi

  def localName(elem: ElemType): String = {
    name(elem).localPart
  }

  def namespaceOption(elem: ElemType): Option[String] = {
    name(elem).namespaceUriOption
  }

  def namespaceAsString(elem: ElemType): String = {
    namespaceOption(elem).getOrElse("")
  }

  def attrOption(elem: ElemType, attributeName: EName): Option[String] = {
    attributes(elem).get(attributeName)
  }

  def attrOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
    attrOption(elem, EName(attributeNamespaceOption, attributeLocalName))
  }

  def attrOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[String] = {
    attrOption(elem, EName(Some(attributeNamespace), attributeLocalName))
  }

  def attrOption(elem: ElemType, attributeLocalName: String): Option[String] = {
    attrOption(elem, EName(None, attributeLocalName))
  }

  def attr(elem: ElemType, attributeName: EName): String = {
    attrOption(elem, attributeName).get
  }

  def attr(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
    attrOption(elem, attributeNamespaceOption, attributeLocalName).get
  }

  def attr(elem: ElemType, attributeNamespace: String, attributeLocalName: String): String = {
    attrOption(elem, attributeNamespace, attributeLocalName).get
  }

  def attr(elem: ElemType, attributeLocalName: String): String = {
    attrOption(elem, attributeLocalName).get
  }

  def normalizedText(elem: ElemType): String = {
    normalizeString(text(elem))
  }

  def trimmedText(elem: ElemType): String = {
    text(elem).trim
  }

  // Private methods

  /**
   * Normalizes the string, removing surrounding whitespace and normalizing internal whitespace to a single space.
   * Whitespace includes #x20 (space), #x9 (tab), #xD (carriage return), #xA (line feed). If there is only whitespace,
   * the empty string is returned. Inspired by the JDOM library.
   */
  private def normalizeString(s: String): String = {
    require(s ne null) // scalastyle:off null

    val separators = Array(' ', '\t', '\r', '\n')
    val words: Seq[String] = s.split(separators).toSeq.filterNot(_.isEmpty)

    words.mkString(" ") // Returns empty string if words.isEmpty
  }
}

object AbstractClarkElemFunctions {

  type Aux[N, E] = AbstractClarkElemFunctions {
    type NodeType = N
    type ElemType = E
  }
}
