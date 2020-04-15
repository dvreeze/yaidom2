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

package eu.cdevreeze.yaidom2.dialect

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.ScopedNodes

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.ListMap

/**
 * General partly implementation of "dialect Scoped elements". This makes implementing each dialect (as Scoped
 * elements) a breeze.
 *
 * In order to avoid type gymnastics, and to offer a simple API to extend in yaidom dialects, this class does not
 * extend type AbstractDialectClarkElem. To the user of this API this does not matter.
 *
 * In order for this type to be useful in dialect element implementations, wrapping an underlying element must be a very
 * fast and non-recursive operation.
 *
 * In concrete element classes extending this trait (directly or indirectly), strongly consider overriding all methods
 * that contain type member ThisElem anywhere in the method signature, by just calling the super-trait version of the method.
 * That would ensure that in those method signatures type member ThisElem has the correct concrete element type.
 *
 * @author Chris de Vreeze
 */
// scalastyle:off number.of.methods
trait AbstractDialectScopedElem extends ScopedNodes.Elem {

  /**
   * Returns the underlying element. This method must be very fast.
   */
  def underlyingElem: ScopedNodes.Elem

  /**
   * Wraps un underlying element. This method must be very fast.
   */
  def wrapElem(underlyingElem: ScopedNodes.Elem): ThisElem

  // ClarkNodes.Elem

  def children: ArraySeq[ThisNode]

  def select(step: ElemStep[ThisElem]): Seq[ThisElem]

  // ElemApi

  def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.filterChildElems(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  def findAllChildElems: Seq[ThisElem] = {
    underlyingElem.findAllChildElems.map(e => wrapElem(e))
  }

  def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findChildElem(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.filterDescendantElems(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  def findAllDescendantElems: Seq[ThisElem] = {
    underlyingElem.findAllDescendantElems.map(e => wrapElem(e))
  }

  def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findDescendantElem(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.filterDescendantElemsOrSelf(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  def findAllDescendantElemsOrSelf: Seq[ThisElem] = {
    underlyingElem.findAllDescendantElemsOrSelf.map(e => wrapElem(e))
  }

  def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findDescendantElemOrSelf(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.findTopmostElems(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.findTopmostElemsOrSelf(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  def findDescendantElemOrSelf(navigationPath: Seq[Int]): Option[ThisElem] = {
    underlyingElem.findDescendantElemOrSelf(navigationPath).map(e => wrapElem(e))
  }

  def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = {
    wrapElem(underlyingElem.getDescendantElemOrSelf(navigationPath))
  }

  // ClarkElemApi

  def name: EName = {
    underlyingElem.name
  }

  def localName: String = {
    underlyingElem.localName
  }

  def namespaceOption: Option[String] = {
    underlyingElem.namespaceOption
  }

  def namespaceAsString: String = {
    underlyingElem.namespaceAsString
  }

  def attributes: ListMap[EName, String] = {
    underlyingElem.attributes
  }

  def attrOption(attributeName: EName): Option[String] = {
    underlyingElem.attrOption(attributeName)
  }

  def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
    underlyingElem.attrOption(attributeNamespaceOption, attributeLocalName)
  }

  def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
    underlyingElem.attrOption(attributeNamespace, attributeLocalName)
  }

  def attrOption(attributeLocalName: String): Option[String] = {
    underlyingElem.attrOption(attributeLocalName)
  }

  def attr(attributeName: EName): String = {
    underlyingElem.attr(attributeName)
  }

  def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
    underlyingElem.attr(attributeNamespaceOption, attributeLocalName)
  }

  def attr(attributeNamespace: String, attributeLocalName: String): String = {
    underlyingElem.attr(attributeNamespace, attributeLocalName)
  }

  def attr(attributeLocalName: String): String = {
    underlyingElem.attr(attributeLocalName)
  }

  def text: String = {
    underlyingElem.text
  }

  def normalizedText: String = {
    underlyingElem.normalizedText
  }

  def trimmedText: String = {
    underlyingElem.trimmedText
  }

  // ScopedElemApi

  def scope: Scope = {
    underlyingElem.scope
  }

  def qname: QName = {
    underlyingElem.qname
  }

  def attributesByQName: ListMap[QName, String] = {
    underlyingElem.attributesByQName
  }

  def textAsQName: QName = {
    underlyingElem.textAsQName
  }

  def textAsResolvedQName: EName = {
    underlyingElem.textAsResolvedQName
  }

  def attrAsQNameOption(attributeName: EName): Option[QName] = {
    underlyingElem.attrAsQNameOption(attributeName)
  }

  def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
    underlyingElem.attrAsQNameOption(attributeNamespaceOption, attributeLocalName)
  }

  def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
    underlyingElem.attrAsQNameOption(attributeNamespace, attributeLocalName)
  }

  def attrAsQName(attributeName: EName): QName = {
    underlyingElem.attrAsQName(attributeName)
  }

  def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
    underlyingElem.attrAsQName(attributeNamespaceOption, attributeLocalName)
  }

  def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
    underlyingElem.attrAsQName(attributeNamespace, attributeLocalName)
  }

  def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
    underlyingElem.attrAsResolvedQNameOption(attributeName)
  }

  def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
    underlyingElem.attrAsResolvedQNameOption(attributeNamespaceOption, attributeLocalName)
  }

  def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
    underlyingElem.attrAsResolvedQNameOption(attributeNamespace, attributeLocalName)
  }

  def attrAsResolvedQName(attributeName: EName): EName = {
    underlyingElem.attrAsResolvedQName(attributeName)
  }

  def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
    underlyingElem.attrAsResolvedQName(attributeNamespaceOption, attributeLocalName)
  }

  def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
    underlyingElem.attrAsResolvedQName(attributeNamespace, attributeLocalName)
  }
}
