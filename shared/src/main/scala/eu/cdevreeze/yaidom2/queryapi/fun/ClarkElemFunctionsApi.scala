package eu.cdevreeze.yaidom2.queryapi.fun

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.queryapi.ElemStep

/**
 * Clark element function API. See `ClarkElemApi`, but this is its non-OO functional counterpart. More precisely,
 * it is the non-OO functional counterpart of `ClarkNodes.Elem`.
 *
 * @author Chris de Vreeze
 */
trait ClarkElemFunctionsApi extends ElemFunctionsApi {

  type ElemType

  type NodeType >: ElemType

  def name(elem: ElemType): EName

  def attributes(elem: ElemType): Iterable[(EName, String)]

  /**
   * Returns the local name. That is, returns the local part of the name of the element.
   * This method must be fast in order to support fast local name queries.
   */
  def localName(elem: ElemType): String

  /**
   * Returns the optional namespace. That is, returns the optional namespace of the name of the element.
   * This method must be fast in order to support fast namespace queries.
   */
  def namespaceOption(elem: ElemType): Option[String]

  /**
   * Returns the equivalent of `namespaceOption.getOrElse("")`.
   * This method must be fast in order to support fast namespace queries.
   */
  def namespaceAsString(elem: ElemType): String

  def attrOption(elem: ElemType, attributeName: EName): Option[String]

  def attrOption(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): Option[String]

  def attrOption(elem: ElemType, attributeNamespace: String, attributeLocalName: String): Option[String]

  /**
   * Finds an attribute that has no namespace by local name, if any.
   */
  def attrOption(elem: ElemType, attributeLocalName: String): Option[String]

  def attr(elem: ElemType, attributeName: EName): String

  def attr(elem: ElemType, attributeNamespaceOption: Option[String], attributeLocalName: String): String

  def attr(elem: ElemType, attributeNamespace: String, attributeLocalName: String): String

  /**
   * Gets an attribute that has no namespace by local name, throwing if no such attribute is found.
   */
  def attr(elem: ElemType, attributeLocalName: String): String

  def text(elem: ElemType): String

  def normalizedText(elem: ElemType): String

  def trimmedText(elem: ElemType): String

  /**
   * Returns all child nodes, of any kind of node (element node, text node etc.).
   */
  def children(elem: ElemType): Seq[NodeType]

  /**
   * Applies the given element step to this element.
   */
  def select(elem: ElemType, step: ElemStep[ElemType]): Seq[ElemType]
}

object ClarkElemFunctionsApi {

  type Aux[E, N] = ClarkElemFunctionsApi {
    type ElemType = E
    type NodeType = N
  }
}
