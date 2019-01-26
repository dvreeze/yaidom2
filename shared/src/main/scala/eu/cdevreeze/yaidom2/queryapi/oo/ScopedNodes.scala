package eu.cdevreeze.yaidom2.queryapi.oo

/**
 * '''Core API''' for element nodes that offer the central `ScopedElemApi` query API. Each element implementation that
 * knows about expanded names as well as qualified name should directly or indirectly implement this API.
 *
 * This API is directly implemented by elements that know about expanded names and about qualified names,
 * but that do not know about their ancestor elements.
 *
 * @author Chris de Vreeze
 */
object ScopedNodes {

  /**
   * Arbitrary node
   */
  trait Node extends ClarkNodes.Node

  /**
   * Potential document child, so an element, processing instruction or comment
   */
  trait CanBeDocumentChild extends Node with ClarkNodes.CanBeDocumentChild

  /**
   * Arbitrary element node, offering the `ScopedElemApi` element query API
   */
  trait Elem extends CanBeDocumentChild with ClarkNodes.Elem with ScopedElemApi {

    type ThisElem <: Elem

    type ThisNode >: ThisElem <: Node
  }

  /**
   * Arbitrary text node
   */
  trait Text extends Node with ClarkNodes.Text

  /**
   * Arbitrary comment node
   */
  trait Comment extends CanBeDocumentChild with ClarkNodes.Comment

  /**
   * Arbitrary processing instruction node
   */
  trait ProcessingInstruction extends CanBeDocumentChild with ClarkNodes.ProcessingInstruction

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
