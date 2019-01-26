package eu.cdevreeze.yaidom2.queryapi.fun.steps

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ElemStep
import eu.cdevreeze.yaidom2.queryapi.fun.BackingElemFunctionsApi
import eu.cdevreeze.yaidom2.queryapi.fun.ClarkElemFunctionsApi
import eu.cdevreeze.yaidom2.queryapi.fun.predicates._

/**
 * Element step factory.
 *
 * This element step factory has been highly inspired by the Saxon 9.9 streaming API. Unlike the Saxon API, this API is Scala-centric
 * instead of Java-centric, this API limits itself to element nodes only, and it is generic.
 *
 * There is an important difference with the Saxon 9.9 API, in that step factories that take a local name expect the
 * name to have no namespace, instead of ignoring the namespace, if any. If the namespace must be ignored, consider
 * using methods that clearly indicate that they indeed ignore the namespace, if any.
 *
 * @author Chris de Vreeze
 */
object ElemSteps {

  // Child axis, for element nodes only

  def childElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](p: E => Boolean)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterChildElems(elem, p) }
  }

  def childElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]]()(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterChildElems(elem, anyElem) }
  }

  def childElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterChildElems(elem, havingName(localName)(ops)) }
  }

  def childElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterChildElems(elem, havingName(namespace, localName)(ops)) }
  }

  def childElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterChildElems(elem, havingName(namespaceOption, localName)(ops)) }
  }

  def childElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](ename: EName)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterChildElems(elem, havingName(ename)(ops)) }
  }

  def childElemsIgnoringNamespace[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterChildElems(elem, havingLocalName(localName)(ops)) }
  }

  // Descendant axis, for element nodes only

  def descendantElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](p: E => Boolean)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElems(elem, p) }
  }

  def descendantElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]]()(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElems(elem, anyElem) }
  }

  def descendantElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElems(elem, havingName(localName)(ops)) }
  }

  def descendantElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElems(elem, havingName(namespace, localName)(ops)) }
  }

  def descendantElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElems(elem, havingName(namespaceOption, localName)(ops)) }
  }

  def descendantElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](ename: EName)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElems(elem, havingName(ename)(ops)) }
  }

  def descendantElemsIgnoringNamespace[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElems(elem, havingLocalName(localName)(ops)) }
  }

  // Descendant-or-self axis, for element nodes only

  def descendantElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](p: E => Boolean)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElemsOrSelf(elem, p) }
  }

  def descendantElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]]()(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElemsOrSelf(elem, anyElem) }
  }

  def descendantElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElemsOrSelf(elem, havingName(localName)(ops)) }
  }

  def descendantElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElemsOrSelf(elem, havingName(namespace, localName)(ops)) }
  }

  def descendantElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElemsOrSelf(elem, havingName(namespaceOption, localName)(ops)) }
  }

  def descendantElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](ename: EName)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElemsOrSelf(elem, havingName(ename)(ops)) }
  }

  def descendantElemsOrSelfIgnoringNamespace[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterDescendantElemsOrSelf(elem, havingLocalName(localName)(ops)) }
  }

  // Descendant axis, short-circuiting, for element nodes only

  def topmostElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](p: E => Boolean)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElems(elem, p) }
  }

  def topmostElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElems(elem, havingName(localName)(ops)) }
  }

  def topmostElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElems(elem, havingName(namespace, localName)(ops)) }
  }

  def topmostElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElems(elem, havingName(namespaceOption, localName)(ops)) }
  }

  def topmostElems[E, F <: ClarkElemFunctionsApi.Aux[E, _]](ename: EName)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElems(elem, havingName(ename)(ops)) }
  }

  def topmostElemsIgnoringNamespace[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElems(elem, havingLocalName(localName)(ops)) }
  }

  // Descendant-or-self axis, short-circuiting, for element nodes only

  def topmostElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](p: E => Boolean)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElemsOrSelf(elem, p) }
  }

  def topmostElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElemsOrSelf(elem, havingName(localName)(ops)) }
  }

  def topmostElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElemsOrSelf(elem, havingName(namespace, localName)(ops)) }
  }

  def topmostElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElemsOrSelf(elem, havingName(namespaceOption, localName)(ops)) }
  }

  def topmostElemsOrSelf[E, F <: ClarkElemFunctionsApi.Aux[E, _]](ename: EName)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElemsOrSelf(elem, havingName(ename)(ops)) }
  }

  def topmostElemsOrSelfIgnoringNamespace[E, F <: ClarkElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findTopmostElemsOrSelf(elem, havingLocalName(localName)(ops)) }
  }

  // Parent axis, for element nodes only

  def parentElem[E, F <: BackingElemFunctionsApi.Aux[E, _]](p: E => Boolean)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findParentElem(elem, p).toIndexedSeq }
  }

  def parentElem[E, F <: BackingElemFunctionsApi.Aux[E, _]]()(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findParentElem(elem, anyElem).toIndexedSeq }
  }

  def parentElem[E, F <: BackingElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findParentElem(elem, havingName(localName)(ops)).toIndexedSeq }
  }

  def parentElem[E, F <: BackingElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findParentElem(elem, havingName(namespace, localName)(ops)).toIndexedSeq }
  }

  def parentElem[E, F <: BackingElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findParentElem(elem, havingName(namespaceOption, localName)(ops)).toIndexedSeq }
  }

  def parentElem[E, F <: BackingElemFunctionsApi.Aux[E, _]](ename: EName)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findParentElem(elem, havingName(ename)(ops)).toIndexedSeq }
  }

  def parentElemIgnoringNamespace[E, F <: BackingElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.findParentElem(elem, havingLocalName(localName)(ops)).toIndexedSeq }
  }

  // Ancestor axis, for element nodes only

  def ancestorElems[E, F <: BackingElemFunctionsApi.Aux[E, _]](p: E => Boolean)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElems(elem, p) }
  }

  def ancestorElems[E, F <: BackingElemFunctionsApi.Aux[E, _]]()(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElems(elem, anyElem) }
  }

  def ancestorElems[E, F <: BackingElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElems(elem, havingName(localName)(ops)) }
  }

  def ancestorElems[E, F <: BackingElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElems(elem, havingName(namespace, localName)(ops)) }
  }

  def ancestorElems[E, F <: BackingElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElems(elem, havingName(namespaceOption, localName)(ops)) }
  }

  def ancestorElems[E, F <: BackingElemFunctionsApi.Aux[E, _]](ename: EName)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElems(elem, havingName(ename)(ops)) }
  }

  def ancestorElemsIgnoringNamespace[E, F <: BackingElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElems(elem, havingLocalName(localName)(ops)) }
  }

  // Ancestor-or-self axis, for element nodes only

  def ancestorElemsOrSelf[E, F <: BackingElemFunctionsApi.Aux[E, _]](p: E => Boolean)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElemsOrSelf(elem, p) }
  }

  def ancestorElemsOrSelf[E, F <: BackingElemFunctionsApi.Aux[E, _]]()(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElemsOrSelf(elem, anyElem) }
  }

  def ancestorElemsOrSelf[E, F <: BackingElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElemsOrSelf(elem, havingName(localName)(ops)) }
  }

  def ancestorElemsOrSelf[E, F <: BackingElemFunctionsApi.Aux[E, _]](namespace: String, localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElemsOrSelf(elem, havingName(namespace, localName)(ops)) }
  }

  def ancestorElemsOrSelf[E, F <: BackingElemFunctionsApi.Aux[E, _]](namespaceOption: Option[String], localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElemsOrSelf(elem, havingName(namespaceOption, localName)(ops)) }
  }

  def ancestorElemsOrSelf[E, F <: BackingElemFunctionsApi.Aux[E, _]](ename: EName)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElemsOrSelf(elem, havingName(ename)(ops)) }
  }

  def ancestorElemsOrSelfIgnoringNamespace[E, F <: BackingElemFunctionsApi.Aux[E, _]](localName: String)(implicit ops: F): ElemStep[E] = {
    { (elem: E) => ops.filterAncestorElemsOrSelf(elem, havingLocalName(localName)(ops)) }
  }
}
