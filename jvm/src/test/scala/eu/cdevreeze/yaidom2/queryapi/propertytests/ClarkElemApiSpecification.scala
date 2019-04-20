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

package eu.cdevreeze.yaidom2.queryapi.propertytests

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.oo.ClarkNodes
import eu.cdevreeze.yaidom2.queryapi.oo.steps.ElemSteps._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

trait ClarkElemApiSpecification[N, E <: ClarkNodes.Elem.Aux[N, E]] extends ElemApiSpecification[E] {
  self: Properties =>

  // "Definitions" of ClarkElemApi methods

  property("localName") = forAll { elem: E =>
    elem.localName == elem.name.localPart
  }

  property("namespaceOption") = forAll { elem: E =>
    elem.namespaceOption == elem.name.namespaceUriOption
  }

  property("namespaceAsString") = forAll { elem: E =>
    elem.namespaceAsString == elem.name.namespaceUriOption.getOrElse("")
  }

  property("attrOption") = forAll { elem: E =>
    val attrNames: Seq[EName] = elem.attributes.keySet.union(bogusAttributeNames).toSeq

    attrNames.map(attrName => elem.attrOption(attrName)) ==
      attrNames.map(attrName => elem.attributes.get(attrName))
  }

  property("attrOption-taking-nsOption-and-localName") = forAll { elem: E =>
    val attrNames: Seq[EName] = elem.attributes.keySet.union(bogusAttributeNames).toSeq

    attrNames.map(attrName => elem.attrOption(attrName.namespaceUriOption, attrName.localPart)) ==
      attrNames.map(attrName => elem.attrOption(attrName))
  }

  property("attrOption-taking-ns-and-localName") = forAll { elem: E =>
    val attrNames: Seq[EName] =
      elem.attributes.keySet.union(bogusAttributeNames).toSeq.filter(_.namespaceUriOption.nonEmpty)

    attrNames.map(attrName => elem.attrOption(attrName.namespaceUriOption.get, attrName.localPart)) ==
      attrNames.map(attrName => elem.attrOption(attrName))
  }

  property("attrOption-no-ns") = forAll { elem: E =>
    val attrNames: Seq[EName] =
      elem.attributes.keySet.union(bogusAttributeNames).toSeq.filter(_.namespaceUriOption.isEmpty)

    attrNames.map(attrName => elem.attrOption(attrName.localPart)) ==
      attrNames.map(attrName => elem.attrOption(attrName))
  }

  property("attr") = forAll { elem: E =>
    val attrNames: Seq[EName] = elem.attributes.keySet.toSeq

    attrNames.map(attrName => elem.attr(attrName)) ==
      attrNames.map(attrName => elem.attributes(attrName))
  }

  property("attr-taking-nsOption-and-localName") = forAll { elem: E =>
    val attrNames: Seq[EName] = elem.attributes.keySet.toSeq

    attrNames.map(attrName => elem.attr(attrName.namespaceUriOption, attrName.localPart)) ==
      attrNames.map(attrName => elem.attr(attrName))
  }

  property("attr-taking-ns-and-localName") = forAll { elem: E =>
    val attrNames: Seq[EName] = elem.attributes.keySet.toSeq.filter(_.namespaceUriOption.nonEmpty)

    attrNames.map(attrName => elem.attr(attrName.namespaceUriOption.get, attrName.localPart)) ==
      attrNames.map(attrName => elem.attr(attrName))
  }

  property("attr-no-ns") = forAll { elem: E =>
    val attrNames: Seq[EName] = elem.attributes.keySet.toSeq.filter(_.namespaceUriOption.isEmpty)

    attrNames.map(attrName => elem.attr(attrName.localPart)) ==
      attrNames.map(attrName => elem.attr(attrName))
  }

  property("text") = forAll { elem: E =>
    elem.text == elem.children.collect { case t: ClarkNodes.Text => t.text }.mkString
  }

  property("normalizedText") = forAll { elem: E =>
    elem.normalizedText == normalizeString(elem.text)
  }

  property("trimmedText") = forAll { elem: E =>
    elem.trimmedText == elem.text.trim
  }

  // Other properties

  property("element-children") = forAll { elem: E =>
    elem.filterChildElems(_ => true) == elem.children.collect { case e: ClarkNodes.Elem => e }
  }

  property("select-children") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(childElems(pred)) == elem.filterChildElems(pred)
  }

  property("select-descendant") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(descendantElems(pred)) == elem.filterDescendantElems(pred)
  }

  property("select-descendant-or-self") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(descendantElemsOrSelf(pred)) == elem.filterDescendantElemsOrSelf(pred)
  }

  property("select-topmost") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(topmostElems(pred)) == elem.findTopmostElems(pred)
  }

  property("select-topmost-or-self") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(topmostElemsOrSelf(pred)) == elem.findTopmostElemsOrSelf(pred)
  }

  property("select-grandchildren") = forAll { (elem: E, pred: E => Boolean) =>
    elem.select(childElems() / childElems(pred)) ==
      elem.filterChildElems(_ => true).flatMap(_.filterChildElems(pred))
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

  private val bogusAttributeNames: Set[EName] = {
    Set(EName(None, "a"), EName("http://www.test.com/", "b"))
  }
}
