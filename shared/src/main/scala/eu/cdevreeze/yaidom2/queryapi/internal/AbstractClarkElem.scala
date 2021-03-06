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

package eu.cdevreeze.yaidom2.queryapi.internal

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.ClarkNodes.Node

/**
 * Abstract partially implemented ClarkNodes.Elem, for re-usable (but overridable) partial element implementations in yaidom2.
 *
 * This is an internal API, although it is visible from the outside. When using this API, keep in mind that the API
 * is not as stable as the purely abstract API.
 *
 * In concrete element classes extending this trait (directly or indirectly), strongly consider overriding all methods
 * that contain type member ThisElem anywhere in the method signature, by just calling the super-trait version of the method.
 * That would ensure that in those method signatures type member ThisElem has the correct concrete element type.
 *
 * @author Chris de Vreeze
 */
trait AbstractClarkElem extends AbstractElem with ClarkNodes.Elem {

  type ThisElem <: AbstractClarkElem.Aux[_, ThisElem]

  type ThisNode >: ThisElem <: Node

  // ClarkElemApi

  def localName: String = {
    // This is an obvious target for overriding for performance
    name.localPart
  }

  def namespaceOption: Option[String] = {
    name.namespaceUriOption
  }

  def namespaceAsString: String = {
    namespaceOption.getOrElse("")
  }

  def attrOption(attributeName: EName): Option[String] = {
    // This is an obvious target for overriding for performance
    attributes.get(attributeName)
  }

  def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
    attrOption(EName(attributeNamespaceOption, attributeLocalName))
  }

  def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
    attrOption(EName(Some(attributeNamespace), attributeLocalName))
  }

  def attrOption(attributeLocalName: String): Option[String] = {
    attrOption(EName(None, attributeLocalName))
  }

  def attr(attributeName: EName): String = {
    attrOption(attributeName).get
  }

  def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
    attrOption(attributeNamespaceOption, attributeLocalName).get
  }

  def attr(attributeNamespace: String, attributeLocalName: String): String = {
    attrOption(attributeNamespace, attributeLocalName).get
  }

  def attr(attributeLocalName: String): String = {
    attrOption(attributeLocalName).get
  }

  def text: String = {
    children.collect { case t: ClarkNodes.Text => t.text }.mkString
  }

  def normalizedText: String = {
    normalizeString(text)
  }

  def trimmedText: String = {
    text.trim
  }

  // ClarkNodes.Elem

  def select(step: ElemStep[ThisElem]): Seq[ThisElem] = {
    step(self)
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

object AbstractClarkElem {

  type Aux[N, E] = AbstractClarkElem {type ThisNode = N; type ThisElem = E}
}
