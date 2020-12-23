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

package eu.cdevreeze.yaidom2.node

/**
 * This package contains an element builder DSL, based on [[eu.cdevreeze.yaidom2.core.StableScope]].
 *
 * First note that in-scope namespaces ([[eu.cdevreeze.yaidom2.core.Scope]]) are quite liberal in allowing
 * undisciplined use of namespace prefixes and default namespaces. In practice, however, very often namespace declarations
 * are only needed at the top level of a document, thus using namespaces and namespace prefixes in a disciplined
 * way (at least within one document). This is where the use of [[eu.cdevreeze.yaidom2.core.StableScope]]
 * comes in. Through its requirements (being an invertible scope when ignoring the default namespace)
 * and its API it enforces a disciplined use of namespaces and their prefixes.
 *
 * The [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Elem]] element implementation is based on
 * [[eu.cdevreeze.yaidom2.core.StableScope]], not on unrestricted [[eu.cdevreeze.yaidom2.core.Scope]].
 * Each such element has a "combined stable scope" computed over all descendants-or-self, being a super-scope
 * of all scopes of descendant-or-self elements. Such elements support a sound element creation API,
 * which is offered by this package.
 *
 * Appreciating the above-mentioned nature of [[eu.cdevreeze.yaidom2.core.StableScope]] and
 * [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Elem]], let's give an example of this element
 * creation API. First set up the "element creator":
 * {{{
 * import eu.cdevreeze.yaidom2.core._
 * import eu.cdevreeze.yaidom2.node.nodebuilder
 *
 * // Assume we have a knownStableScope of type StableScope, to start element creation with.
 * // The decision whether or not to use a default namespace is visible in this StableScope.
 * // Here it is assumed that there is a default namespace, namely "http://www.w3.org/1999/xhtml".
 * // Commonly needed namespaces in the document to create should also be there.
 *
 * val elemCreator: nodebuilder.NodeBuilderCreator = nodebuilder.NodeBuilderCreator(knownStableScope)
 * import elemCreator._
 * }}}
 *
 * Below a small snippet of XBRL XML is created (see www.xbrl.org, in particular the iXBRL conformance suite),
 * as the example XML. It requires namespace prefix "xbrli" for namespace "http://www.xbrl.org/2003/instance".
 * Assume that this prefix-namespace mapping is indeed in the knownStableScope mentioned above.
 * Let's create some (XBRL) XML:
 * {{{
 * def makeXbrliContext(id: String, startDate: LocalDate, endDate: LocalDate): nodebuilder.Elem = {
 *   emptyElem(q"xbrli:context")
 *     .plusAttribute(q"id", id)
 *     .plusChildElem(
 *       elem(
 *         q"xbrli:entity",
 *         Seq(textElem(q"xbrli:identifier", ListMap(q"scheme" -> "test"), "Test Co 1").elem)
 *       )
 *     )
 *     .plusChildElem(
 *       elem(
 *         q"xbrli:period",
 *         Seq(
 *           textElem(q"xbrli:startdate", startDate.toString).elem,
 *           textElem(q"xbrli:enddate", endDate.toString).elem,
 *         )
 *       )
 *     )
 *     .elem
 * }
 * }}}
 *
 * Note that 2 "elem" functions are used here: one in [[eu.cdevreeze.yaidom2.creationapi.ElemCreationApi]]
 * and one in [[eu.cdevreeze.yaidom2.creationapi.ElemInKnownScope]] (the latter for "unwrapping", returning
 * a [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Elem]]). Also note that element and attribute names
 * in this element creation API must be of type [[eu.cdevreeze.yaidom2.core.QName]] instead of String.
 *
 * The result of the code above could be XML (as nodebuilder.Elem) like this:
 * {{{
 * <xbrli:context id="DUR-1">
 *   <xbrli:entity>
 *     <xbrli:identifier scheme="test">Test Co 1</xbrli:identifier>
 *   </xbrli:entity>
 *   <xbrli:period>
 *     <xbrli:startdate>2005-12-31</xbrli:startdate>
 *     <xbrli:enddate>2006-12-31</xbrli:enddate>
 *   </xbrli:period>
 * </xbrli:context>
 * }}}
 *
 * The code above did not introduce any new unknown namespaces and their prefixes. Sometimes we need
 * to introduce namespaces in an ad-hoc manner (hopefully having a corresponding prefix as well from some
 * input to the program):
 * {{{
 *  def makeXbrliUnit(id: String, measure: QName, neededScopeForMeasure: StableScope): nodebuilder.Elem = {
 *    require(
 *      knownStableScope.appendNonConflictingScope(neededScopeForMeasure).resolveQNameOption(measure).nonEmpty)
 *
 *    emptyElem(q"xbrli:unit")
 *      .plusAttribute(q"id", id)
 *      .plusChildElem(textElem(q"xbrli:measure", measure.toString))
 *      .usingExtraScope(neededScopeForMeasure)
 *      .elem
 *  }
 * }}}
 *
 * The call to method "usingExtraScope" introduces the namespace needed to resolve the QName content of
 * the "xbrli:measure" element, and makes sure the the "xbrli:unit" element and its desdendants have that
 * namespace in scope.
 *
 * The result of the code above could be XML (as nodebuilder.Elem) like this:
 * {{{
 * <xbrli:unit id="u1">
 *   <xbrli:measure>iso4217:GBP</xbrli:measure>
 * </xbrli:unit>
 * }}}
 *
 * Building snippets of XML this way probably leads to a mess of namespace declarations and undeclarations,
 * yet without introducing any namespace conflicts. This is ok, but must be fixed in a final step. To that
 * end, use function [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.ElemInKnownScope.havingSameScopeInDescendantsOrSelf]].
 * The snippets of XML created above are used in a complete X(HT)ML document as follows:
 * {{{
 * val xbrliContext1: nodebuilder.Elem =
 *   makeXbrliContext("DUR-1", LocalDate.parse("2005-12-31"), LocalDate.parse("2006-12-31"))
 * val xbrliContext2: nodebuilder.Elem =
 *   makeXbrliContext("NFC1", LocalDate.parse("2005-01-01"), LocalDate.parse("2005-12-31"))
 *
 * val xbrliUnit1: nodebuilder.Elem = makeXbrliUnit("u1", q"iso4217:GBP", StableScope.empty)
 * val xbrliUnit2: nodebuilder.Elem = makeXbrliUnit("ID-PURE", q"xbrli:pure", StableScope.empty)
 * val xbrliUnit3: nodebuilder.Elem = makeXbrliUnit("GBP", q"iso4217:GBP", StableScope.empty)
 *
 * val xhtml: nodebuilder.Elem =
 *   emptyElem(q"html")
 *     .plusChildElem(
 *       elem(
 *         q"head",
 *         Seq(
 *           emptyElem(q"meta", ListMap(q"content" -> "text/html; charset=UTF-8", q"http-equiv" -> "Content-Type"))
 *             .elem,
 *           textElem(q"title", "Basic Inline XBRL Example").elem
 *         )
 *       )
 *     )
 *     .plusChildElem(
 *       emptyElem(q"body", ListMap(q"xml:lang" -> "en"))
 *         .plusChildElem(
 *           emptyElem(q"div", ListMap(q"style" -> "display:none"))
 *             .plusChildElem(
 *               emptyElem(q"ix:header")
 *                 .plusChildElem(
 *                   elem(
 *                     q"ix:references",
 *                     Seq(emptyElem(
 *                       q"link:schemaref",
 *                       ListMap(
 *                         q"xlink:href" -> "../../schemas/ch/pt/2004-12-01/uk-gaap-pt-2004-12-01.xsd",
 *                         q"xlink:type" -> "simple")).elem)
 *                   )
 *                 )
 *                 .plusChildElem(
 *                   emptyElem(q"ix:resources")
 *                     .plusChild(xbrliContext1)
 *                     .plusChild(xbrliContext2)
 *                     .plusChild(xbrliUnit1)
 *                     .plusChild(xbrliUnit2)
 *                     .plusChild(xbrliUnit3)
 *                 )
 *             )
 *         )
 *         .plusChildElem(
 *           elem(
 *             q"ix:nonnumeric",
 *             ListMap(q"contextref" -> "DUR-1", q"name" -> "pt:DescriptionAddressAssociate"),
 *             Seq(
 *               elem(
 *                 q"b",
 *                 Seq(
 *                   nodebuilder.NodeBuilders.Text("   A string of text.   "),
 *                   elem(
 *                     q"ix:exclude",
 *                     Seq(
 *                       textElem(q"i", "   A number. 1,234,456.78   ").elem
 *                     )
 *                   ).elem,
 *                   nodebuilder.NodeBuilders.Text("   More text>   "),
 *                 )
 *               ).elem
 *             )
 *           ).usingExtraScope(StableScope.from("pt" -> "http://www.xbrl.org/uk/fr/gaap/pt/2004-12-01"))
 *         )
 *     )
 *     .havingSameScopeInDescendantsOrSelf
 *     .elem
 * }}}
 * Note the ad-hoc insertion of the namespace with prefix "pt", and the "finishing touch" with method "havingSameScopeInDescendantsOrSelf".
 *
 * The element creation API based on [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Elem]], which in turn is based on
 * [[eu.cdevreeze.yaidom2.core.StableScope]], is backed by a small "mathematical theory". This theory shows for example that
 * each such element can have all namespace declarations at the root level, without exception.
 *
 * One way to look at the use of the element creation API, as shown above, is as follows: all intermediate
 * results are of type [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.ElemInKnownScope]], where
 * each instance holds an element (of type [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Elem]])
 * and a known [[eu.cdevreeze.yaidom2.core.StableScope]], where the "combined stable scope" of the element
 * is always a "compatible sub-scope" of the latter stable scope. When method
 * [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.ElemInKnownScope.usingExtraScope]] is called
 * (directly or indirectly) both the element scope and known scope may "grow".
 *
 * @author Chris de Vreeze
 */
package object nodebuilder {

  type Document = NodeBuilderDocument

  type Node = NodeBuilders.Node

  type CanBeDocumentChild = NodeBuilders.CanBeDocumentChild

  type Elem = NodeBuilders.Elem

  type Text = NodeBuilders.Text

  type Comment = NodeBuilders.Comment

  type ProcessingInstruction = NodeBuilders.ProcessingInstruction

  val Document = NodeBuilderDocument

  val Node = NodeBuilders.Node

  val Elem = NodeBuilders.Elem

  // Wrappers

  type ElemInKnownScope = NodeBuilders.ElemInKnownScope

  val ElemInKnownScope = NodeBuilders.ElemInKnownScope
}
