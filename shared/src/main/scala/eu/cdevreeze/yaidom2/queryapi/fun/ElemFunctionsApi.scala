package eu.cdevreeze.yaidom2.queryapi.fun

/**
 * Element function API. See `ElemApi`, but this is its non-OO functional counterpart.
 *
 * @author Chris de Vreeze
 */
trait ElemFunctionsApi {

  type ElemType

  def filterChildElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findChildElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterDescendantElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findDescendantElem(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def filterDescendantElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findDescendantElemOrSelf(elem: ElemType, p: ElemType => Boolean): Option[ElemType]

  def findTopmostElems(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]

  def findTopmostElemsOrSelf(elem: ElemType, p: ElemType => Boolean): Seq[ElemType]
}

object ElemFunctionsApi {

  type Aux[E] = ElemFunctionsApi {
    type ElemType = E
  }
}
