package eu.cdevreeze.yaidom2.queryapi.oo

/**
 * '''Core API''' for element nodes that offer the central `BackingElemApi` query API (and more). Each element implementation that
 * knows about expanded names as well as qualified name and that also knows about ancestor elements, should directly or indirectly
 * implement this API.
 *
 * This API is directly implemented by elements that are used as '''backing elements''' in "yaidom dialects".
 * The yaidom2 dialects use this abstract backing element API, thus allowing for multiple backing element
 * implementation behind an yaidom2 XML dialect.
 *
 * Efficient implementations are possible for indexed elements and Saxon NodeInfo objects (backed by Saxon native tiny trees).
 * Saxon-backed elements are not offered by core yaidom2, however. Saxon tiny trees are attractive for their low memory
 * footprint and querying performance.
 *
 * @author Chris de Vreeze
 */
object BackingNodes {

  /**
   * Arbitrary node
   */
  trait Node extends ScopedNodes.Node

  /**
   * Potential document child, so an element, processing instruction or comment
   */
  trait CanBeDocumentChild extends Node with ScopedNodes.CanBeDocumentChild

  /**
   * Arbitrary element node, offering the `BackingElemApi` element query API.
   */
  trait Elem extends CanBeDocumentChild with ScopedNodes.Elem with BackingElemApi {

    type ThisElem <: Elem

    type ThisNode >: ThisElem <: Node
  }

  /**
   * Arbitrary text node
   */
  trait Text extends Node with ScopedNodes.Text

  /**
   * Arbitrary comment node
   */
  trait Comment extends CanBeDocumentChild with ScopedNodes.Comment

  /**
   * Arbitrary processing instruction node
   */
  trait ProcessingInstruction extends CanBeDocumentChild with ScopedNodes.ProcessingInstruction

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
