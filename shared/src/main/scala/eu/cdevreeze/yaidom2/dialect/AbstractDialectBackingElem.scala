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

import java.net.URI

import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.queryapi.BackingNodes
import eu.cdevreeze.yaidom2.queryapi.ElemStep

/**
 * General partly implementation of "dialect Backing elements". This makes implementing each dialect (as Backing
 * elements) a breeze.
 *
 * In order to avoid type gymnastics, and to offer a simple API to extend in yaidom dialects, this class does not
 * extend type AbstractDialectScopedElem. To the user of this API this does not matter.
 *
 * In order for this type to be useful in dialect element implementations, wrapping an underlying element must be a very
 * fast and non-recursive operation.
 *
 * @author Chris de Vreeze
 */
// scalastyle:off number.of.methods
trait AbstractDialectBackingElem extends BackingNodes.Elem {

  /**
   * Returns the underlying element. This method must be very fast.
   */
  def underlyingElem: BackingNodes.Elem

  /**
   * Wraps un underlying element. This method must be very fast.
   */
  def wrapElem(underlyingElem: BackingNodes.Elem): ThisElem

  // ClarkNodes.Elem

  def children: ArraySeq[ThisNode]

  def select(step: ElemStep[ThisElem]): Seq[ThisElem]

  // ElemApi

  final def filterChildElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.filterChildElems(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findAllChildElems(): Seq[ThisElem] = {
    underlyingElem.findAllChildElems().map(e => wrapElem(e))
  }

  final def findChildElem(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findChildElem(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def filterDescendantElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.filterDescendantElems(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findAllDescendantElems(): Seq[ThisElem] = {
    underlyingElem.findAllDescendantElems().map(e => wrapElem(e))
  }

  final def findDescendantElem(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findDescendantElem(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def filterDescendantElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.filterDescendantElemsOrSelf(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findAllDescendantElemsOrSelf(): Seq[ThisElem] = {
    underlyingElem.findAllDescendantElemsOrSelf().map(e => wrapElem(e))
  }

  final def findDescendantElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findDescendantElemOrSelf(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findTopmostElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.findTopmostElems(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findTopmostElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.findTopmostElemsOrSelf(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findDescendantElemOrSelf(navigationPath: Seq[Int]): Option[ThisElem] = {
    underlyingElem.findDescendantElemOrSelf(navigationPath).map(e => wrapElem(e))
  }

  final def getDescendantElemOrSelf(navigationPath: Seq[Int]): ThisElem = {
    wrapElem(underlyingElem.getDescendantElemOrSelf(navigationPath))
  }

  // ClarkElemApi

  final def name: EName = {
    underlyingElem.name
  }

  final def localName: String = {
    underlyingElem.localName
  }

  final def namespaceOption: Option[String] = {
    underlyingElem.namespaceOption
  }

  final def namespaceAsString: String = {
    underlyingElem.namespaceAsString
  }

  final def attributes: SeqMap[EName, String] = {
    underlyingElem.attributes
  }

  final def attrOption(attributeName: EName): Option[String] = {
    underlyingElem.attrOption(attributeName)
  }

  final def attrOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String] = {
    underlyingElem.attrOption(attributeNamespaceOption, attributeLocalName)
  }

  final def attrOption(attributeNamespace: String, attributeLocalName: String): Option[String] = {
    underlyingElem.attrOption(attributeNamespace, attributeLocalName)
  }

  final def attrOption(attributeLocalName: String): Option[String] = {
    underlyingElem.attrOption(attributeLocalName)
  }

  final def attr(attributeName: EName): String = {
    underlyingElem.attr(attributeName)
  }

  final def attr(attributeNamespaceOption: Option[String], attributeLocalName: String): String = {
    underlyingElem.attr(attributeNamespaceOption, attributeLocalName)
  }

  final def attr(attributeNamespace: String, attributeLocalName: String): String = {
    underlyingElem.attr(attributeNamespace, attributeLocalName)
  }

  final def attr(attributeLocalName: String): String = {
    underlyingElem.attr(attributeLocalName)
  }

  final def text: String = {
    underlyingElem.text
  }

  final def normalizedText: String = {
    underlyingElem.normalizedText
  }

  final def trimmedText: String = {
    underlyingElem.trimmedText
  }

  // ScopedElemApi

  final def scope: Scope = {
    underlyingElem.scope
  }

  final def qname: QName = {
    underlyingElem.qname
  }

  final def attributesByQName: SeqMap[QName, String] = {
    underlyingElem.attributesByQName
  }

  final def textAsQName: QName = {
    underlyingElem.textAsQName
  }

  final def textAsResolvedQName: EName = {
    underlyingElem.textAsResolvedQName
  }

  final def attrAsQNameOption(attributeName: EName): Option[QName] = {
    underlyingElem.attrAsQNameOption(attributeName)
  }

  final def attrAsQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName] = {
    underlyingElem.attrAsQNameOption(attributeNamespaceOption, attributeLocalName)
  }

  final def attrAsQNameOption(attributeNamespace: String, attributeLocalName: String): Option[QName] = {
    underlyingElem.attrAsQNameOption(attributeNamespace, attributeLocalName)
  }

  final def attrAsQName(attributeName: EName): QName = {
    underlyingElem.attrAsQName(attributeName)
  }

  final def attrAsQName(attributeNamespaceOption: Option[String], attributeLocalName: String): QName = {
    underlyingElem.attrAsQName(attributeNamespaceOption, attributeLocalName)
  }

  final def attrAsQName(attributeNamespace: String, attributeLocalName: String): QName = {
    underlyingElem.attrAsQName(attributeNamespace, attributeLocalName)
  }

  final def attrAsResolvedQNameOption(attributeName: EName): Option[EName] = {
    underlyingElem.attrAsResolvedQNameOption(attributeName)
  }

  final def attrAsResolvedQNameOption(attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName] = {
    underlyingElem.attrAsResolvedQNameOption(attributeNamespaceOption, attributeLocalName)
  }

  final def attrAsResolvedQNameOption(attributeNamespace: String, attributeLocalName: String): Option[EName] = {
    underlyingElem.attrAsResolvedQNameOption(attributeNamespace, attributeLocalName)
  }

  final def attrAsResolvedQName(attributeName: EName): EName = {
    underlyingElem.attrAsResolvedQName(attributeName)
  }

  final def attrAsResolvedQName(attributeNamespaceOption: Option[String], attributeLocalName: String): EName = {
    underlyingElem.attrAsResolvedQName(attributeNamespaceOption, attributeLocalName)
  }

  final def attrAsResolvedQName(attributeNamespace: String, attributeLocalName: String): EName = {
    underlyingElem.attrAsResolvedQName(attributeNamespace, attributeLocalName)
  }

  // BackingElemApi

  final def findParentElem(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findParentElem(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findParentElem(): Option[ThisElem] = {
    underlyingElem.findParentElem().map(e => wrapElem(e))
  }

  final def filterAncestorElems(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.filterAncestorElems(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findAllAncestorElems(): Seq[ThisElem] = {
    underlyingElem.findAllAncestorElems().map(e => wrapElem(e))
  }

  final def findAncestorElem(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findAncestorElem(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def filterAncestorElemsOrSelf(p: ThisElem => Boolean): Seq[ThisElem] = {
    underlyingElem.filterAncestorElemsOrSelf(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findAllAncestorElemsOrSelf(): Seq[ThisElem] = {
    underlyingElem.findAllAncestorElemsOrSelf().map(e => wrapElem(e))
  }

  final def findAncestorElemOrSelf(p: ThisElem => Boolean): Option[ThisElem] = {
    underlyingElem.findAncestorElemOrSelf(e => p(wrapElem(e))).map(e => wrapElem(e))
  }

  final def findAllPrecedingSiblingElems(): Seq[ThisElem] = {
    underlyingElem.findAllPrecedingSiblingElems().map(e => wrapElem(e))
  }

  final def ownNavigationPathRelativeToRootElem: Seq[Int] = {
    underlyingElem.ownNavigationPathRelativeToRootElem
  }

  final def baseUriOption: Option[URI] = {
    underlyingElem.baseUriOption
  }

  final def baseUri: URI = {
    underlyingElem.baseUri
  }

  final def docUriOption: Option[URI] = {
    underlyingElem.docUriOption
  }

  final def docUri: URI = {
    underlyingElem.docUri
  }

  final def rootElem: ThisElem = {
    wrapElem(underlyingElem.rootElem)
  }
}
