package eu.cdevreeze.yaidom2.queryapi.oo

import eu.cdevreeze.yaidom2.queryapi.ElemStep

/**
 * '''Core API''' for element nodes that offer the central `ClarkElemApi` query API. Each element implementation should
 * directly or indirectly implement this API.
 *
 * This API is directly implemented by elements that know about expanded names but not about qualified names.
 *
 * @author Chris de Vreeze
 */
object ClarkNodes {

  /**
   * Arbitrary node
   */
  trait Node extends Nodes.Node

  /**
   * Potential document child, so an element, processing instruction or comment
   */
  trait CanBeDocumentChild extends Node with Nodes.CanBeDocumentChild

  /**
   * Arbitrary element node, offering the `ClarkElemApi` element query API (and more)
   */
  trait Elem extends CanBeDocumentChild with Nodes.Elem with ClarkElemApi {

    type ThisElem <: Elem

    type ThisNode >: ThisElem <: Node

    /**
     * Returns all child nodes, of any kind of node (element node, text node etc.).
     */
    def children: Seq[ThisNode]

    /**
     * Applies the given element step to this element.
     */
    def select(step: ElemStep[ThisElem]): Seq[ThisElem]
  }

  /**
   * Arbitrary text node
   */
  trait Text extends Node with Nodes.Text

  /**
   * Arbitrary comment node
   */
  trait Comment extends CanBeDocumentChild with Nodes.Comment

  /**
   * Arbitrary processing instruction node
   */
  trait ProcessingInstruction extends CanBeDocumentChild with Nodes.ProcessingInstruction

  object Elem {

    /**
     * This query API type, restricting Node and Elem to the passed type parameters.
     *
     * @tparam N The node type
     * @tparam E The element type
     */
    type Aux[N, E] = Elem { type ThisNode = N; type ThisElem = E }
  }
}
