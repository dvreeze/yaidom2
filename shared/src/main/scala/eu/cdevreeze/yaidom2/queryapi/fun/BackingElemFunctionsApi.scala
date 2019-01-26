package eu.cdevreeze.yaidom2.queryapi.fun

import java.net.URI

/**
 * Backing element function API. See `BackingElemApi`, but this is its non-OO functional counterpart. More precisely,
 * it is the non-OO functional counterpart of `BackingNodes.Elem`.
 *
 * @author Chris de Vreeze
 */
trait BackingElemFunctionsApi extends ScopedElemFunctionsApi {

  type ElemType

  def findParentElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterAncestorElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAncestorElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterAncestorElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findAncestorElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  /**
   * Returns the optional base URI, computed from the document URI, if any, and the XML base attributes of the
   * ancestors, if any.
   */
  def baseUriOption(elem: ElemType): Option[URI]

  /**
   * The base URI, defaulting to the empty URI if absent
   */
  def baseUri(elem: ElemType): URI

  /**
   * The optional document URI of the containing document, if any
   */
  def docUriOption(elem: ElemType): Option[URI]

  /**
   * The document URI, defaulting to the empty URI if absent
   */
  def docUri(elem: ElemType): URI

  /**
   * The root element
   */
  def rootElem(elem: ElemType): ElemType
}

object BackingElemFunctionsApi {

  type Aux[E, N] = BackingElemFunctionsApi {
    type ElemType = E
    type NodeType = N
  }
}
