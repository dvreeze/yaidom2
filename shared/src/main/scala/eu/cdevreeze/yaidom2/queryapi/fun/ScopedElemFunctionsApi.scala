package eu.cdevreeze.yaidom2.queryapi.fun

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope

/**
 * Scoped element function API. See `ScopedElemApi`, but this is its non-OO functional counterpart. More precisely,
 * it is the non-OO functional counterpart of `ScopedNodes.Elem`.
 *
 * @author Chris de Vreeze
 */
trait ScopedElemFunctionsApi extends ClarkElemFunctionsApi {

  type ElemType

  def scope(elem: ElemType): Scope

  def qname(elem: ElemType): QName

  def attributesByQName(elem: ElemType): Iterable[(QName, String)]

  def textAsQName(elem: ElemType): QName

  def textAsResolvedQName(elem: ElemType): EName

  def attrAsQNameOption(elem: ElemType, attributeName: EName): Option[QName]

  def attrAsQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[QName]

  def attrAsQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[QName]

  def attrAsQName(elem: ElemType, attributeName: EName): QName

  def attrAsQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): QName

  def attrAsQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): QName

  def attrAsResolvedQNameOption(elem: ElemType, attributeName: EName): Option[EName]

  def attrAsResolvedQNameOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[EName]

  def attrAsResolvedQNameOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[EName]

  def attrAsResolvedQName(elem: ElemType, attributeName: EName): EName

  def attrAsResolvedQName(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): EName

  def attrAsResolvedQName(elem: ElemType, attributeNamespace: String, attributeLocalName: String): EName
}

object ScopedElemFunctionsApi {

  type Aux[E, N] = ScopedElemFunctionsApi {
    type ElemType = E
    type NodeType = N
  }
}
