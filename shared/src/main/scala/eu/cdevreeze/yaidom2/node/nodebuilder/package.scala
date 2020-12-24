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
 *     .plusChild(
 *       elem(
 *         q"xbrli:entity",
 *         Seq(textElem(q"xbrli:identifier", ListMap(q"scheme" -> "test"), "Test Co 1"))
 *       )
 *     )
 *     .plusChild(
 *       elem(
 *         q"xbrli:period",
 *         Seq(
 *           textElem(q"xbrli:startdate", startDate.toString),
 *           textElem(q"xbrli:enddate", endDate.toString),
 *         )
 *       )
 *     )
 * }
 * }}}
 *
 * Note that element and attribute names in this element creation API must be of type [[eu.cdevreeze.yaidom2.core.QName]]
 * instead of String.
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
 * def makeXbrliUnit(id: String, measure: QName, neededScopeForMeasure: StableScope): nodebuilder.Elem = {
 *   require(knownStableScope.appendNonConflictingScope(neededScopeForMeasure).resolveQNameOption(measure)
 *     .nonEmpty)
 *
 *   emptyElem(q"xbrli:unit")
 *     .plusAttribute(q"id", id)
 *     .plusChild(textElem(q"xbrli:measure", measure.toString))
 *     .usingExtraScopeDeeply(neededScopeForMeasure)
 * }
 * }}}
 *
 * The call to method "usingExtraScopeDeeply" introduces the namespace needed to resolve the QName content of
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
 * end, use function [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Elem.havingSameScopeInDescendantsOrSelf]].
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
 * // Note that there are some surprising attributes below (for those knowing XBRL but not so much iXBRL):
 * // Attribute xsi:schemaLocation here uses only lower-case characters; same for contextRef etc.
 *
 * val xhtml: nodebuilder.Elem =
 *   emptyElem(q"html")
 *     .plusChild(
 *       elem(
 *         q"head",
 *         Seq(
 *           emptyElem(
 *             q"meta",
 *             ListMap(q"content" -> "text/html; charset=UTF-8", q"http-equiv" -> "Content-Type")),
 *           textElem(q"title", "Basic Inline XBRL Example")
 *         )
 *       )
 *     )
 *     .plusChild(
 *       emptyElem(q"body", ListMap(q"xml:lang" -> "en"))
 *         .plusChild(
 *           emptyElem(q"div", ListMap(q"style" -> "display:none"))
 *             .plusChild(
 *               emptyElem(q"ix:header")
 *                 .plusChild(
 *                   elem(
 *                     q"ix:references",
 *                     Seq(emptyElem(
 *                       q"link:schemaref",
 *                       ListMap(
 *                         q"xlink:href" -> "../../schemas/ch/pt/2004-12-01/uk-gaap-pt-2004-12-01.xsd",
 *                         q"xlink:type" -> "simple")))
 *                   )
 *                 )
 *                 .plusChild(
 *                   emptyElem(q"ix:resources")
 *                     .plusChild(xbrliContext1)
 *                     .plusChild(xbrliContext2)
 *                     .plusChild(xbrliUnit1)
 *                     .plusChild(xbrliUnit2)
 *                     .plusChild(xbrliUnit3)
 *                 )
 *             )
 *         )
 *         .plusChild(
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
 *                       textElem(q"i", "   A number. 1,234,456.78   ")
 *                     )
 *                   ),
 *                   nodebuilder.NodeBuilders.Text("   More text>   "),
 *                 )
 *               )
 *             )
 *           ).usingExtraScopeDeeply(StableScope.from("pt" -> "http://www.xbrl.org/uk/fr/gaap/pt/2004-12-01"))
 *         )
 *     )
 *     .havingSameScopeInDescendantsOrSelf
 * }}}
 * Note the ad-hoc insertion of the namespace with prefix "pt", and the "finishing touch" with method "havingSameScopeInDescendantsOrSelf".
 * Also note that this namespace prefix was "inserted" afterwards, which is ok, because it is not an element or attribute QName
 * that would have to be resolved immediately on construction of the element.
 *
 * Often it is needed to take control of inserting prefix-namespace mappings (whether already known or not).
 * See the following example, in which several methods are used to insert "sub-scopes", in order to resolve
 * element or attribute QNames instead of having exceptions thrown:
 * {{{
 * val LinkNs = "http://www.xbrl.org/2003/linkbase"
 * val RefNs = "http://www.xbrl.org/2006/ref"
 * val XLinkNs = "http://www.w3.org/1999/xlink"
 * val XsiNs = "http://www.w3.org/2001/XMLSchema-instance"
 *
 * val nodeBuilderDoc: nodebuilder.Document = {
 *   val scope = StableScope.from("link" -> LinkNs, "ref" -> RefNs, "xlink" -> XLinkNs, "xsi" -> XsiNs)
 *   val elemCreator = NodeBuilderCreator(scope)
 *
 *   import elemCreator._
 *
 *   // Below, note the different ways in which created elements are made aware of extra namespaces.
 *   // The ElemCreationApi methods pretty much "take care of themselves" when it comes to namespaces
 *   // of element and attribute QNames. Therefore we did not have to introduce the "ref" namespaces below.
 *   // For the creationapi.Elem methods this is not quite the case, as seen below.
 *
 *   val elm: nodebuilder.Elem = emptyElem(q"link:namespace")
 *     .withNeededExtraScope(Set(q"xsi:schemaLocation"), scope)
 *     .plusAttribute(
 *       q"xsi:schemaLocation",
 *       "http://www.xbrl.org/2006/ref http://www.xbrl.org/2006/ref-2006-02-27.xsd")
 *     .plusChild {
 *       emptyElem(q"link:referenceLink")
 *         .withNeededExtraScope(Set(q"xlink:type"), scope)
 *         .plusAttribute(q"xlink:role", "http://www.xbrl.org/2003/role/link")
 *         .plusAttribute(q"xlink:type", "extended")
 *         .plusChild {
 *           emptyElem(q"link:loc")
 *             .withExtraScope(scope.filterKeysCompatibly(Set("xlink")))
 *             .plusAttribute(q"xlink:href", "jenv-bw2-axes.xsd#jenv-bw2-dim_LiabilitiesOtherAxis")
 *             .plusAttribute(q"xlink:label", "jenv-bw2-dim_LiabilitiesOtherAxis_loc")
 *             .plusAttribute(q"xlink:type", "locator")
 *         }
 *         .plusChild {
 *           emptyElem(q"link:loc")
 *             .withExtraScope(scope.filterKeysCompatibly(_ == "xlink"))
 *             .plusAttribute(q"xlink:href", "jenv-bw2-axes.xsd#jenv-bw2-dim_LoansAdvancesGuaranteesAxis")
 *             .plusAttribute(q"xlink:label", "jenv-bw2-dim_LoansAdvancesGuaranteesAxis_loc")
 *             .plusAttribute(q"xlink:type", "locator")
 *         }
 *         .plusChild {
 *           emptyElem(q"link:loc", scope.filterKeysCompatibly(Set("xlink")))
 *             .plusAttribute(
 *               q"xlink:href",
 *               "jenv-bw2-axes.xsd#jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis")
 *             .plusAttribute(q"xlink:label", "jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis_loc")
 *             .plusAttribute(q"xlink:type", "locator")
 *         }
 *         .plusChild {
 *           emptyElem(q"link:reference")
 *             .withNeededExtraScope(Set(q"xlink:type"), scope.filterKeysCompatibly(Set("xlink")))
 *             .plusAttribute(q"id", "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
 *             .plusAttribute(q"xlink:label", "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
 *             .plusAttribute(q"xlink:role", "http://www.xbrl.org/2003/role/reference")
 *             .plusAttribute(q"xlink:type", "resource")
 *             .plusChild(textElem(q"ref:Article", "383e"))
 *             .plusChild(textElem(q"ref:IssueDate", "2019-01-01"))
 *             .plusChild(textElem(q"ref:Name", "Burgerlijk wetboek boek 2"))
 *         }
 *         .plusChild {
 *           emptyElem(q"link:reference")
 *             .usingExtraScopeDeeply(scope.filterKeysCompatibly(Set("xlink")))
 *             .plusAttribute(q"id", "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
 *             .plusAttribute(q"xlink:label", "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
 *             .plusAttribute(q"xlink:role", "http://www.xbrl.org/2003/role/reference")
 *             .plusAttribute(q"xlink:type", "resource")
 *             .plusChild(textElem(q"ref:Chapter", "115"))
 *             .plusChild(textElem(q"ref:IssueDate", "2019-01-01"))
 *             .plusChild(textElem(q"ref:Name", "Richtlijnen voor de jaarverslaggeving"))
 *             .plusChild(textElem(q"ref:Paragraph", "214"))
 *         }
 *         .plusChild {
 *           emptyElem(q"link:reference")
 *             .withNeededExtraScope(Set(q"xlink:type"), scope.filterKeysCompatibly(Set("xlink")))
 *             .plusAttribute(q"id", "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
 *             .plusAttribute(q"xlink:label", "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
 *             .plusAttribute(q"xlink:role", "http://www.xbrl.org/2003/role/reference")
 *             .plusAttribute(q"xlink:type", "resource")
 *             .plusChild(textElem(q"ref:Chapter", "610"))
 *             .plusChild(textElem(q"ref:IssueDate", "2019-01-01"))
 *             .plusChild(textElem(q"ref:Name", "Richtlijnen voor de jaarverslaggeving"))
 *             .plusChild(textElem(q"ref:Paragraph", "106"))
 *         }
 *         .plusChild {
 *           emptyElem(q"link:referenceArc")
 *             .withNeededExtraScope(Set(q"xlink:type"), scope.filterKeysCompatibly(Set("xlink")))
 *             .plusAttribute(q"xlink:arcrole", "http://www.xbrl.org/2003/arcrole/concept-reference")
 *             .plusAttribute(q"xlink:from", "jenv-bw2-dim_LiabilitiesOtherAxis_loc")
 *             .plusAttribute(q"xlink:to", "jenv-bw2-dim_RJ_2019-01-01_610_106_ref")
 *             .plusAttribute(q"xlink:type", "arc")
 *         }
 *         .plusChild {
 *           emptyElem(q"link:referenceArc")
 *             .withNeededExtraScope(Set(q"xlink:type"), scope.filterKeysCompatibly(Set("xlink")))
 *             .plusAttribute(q"xlink:arcrole", "http://www.xbrl.org/2003/arcrole/concept-reference")
 *             .plusAttribute(q"xlink:from", "jenv-bw2-dim_LoansAdvancesGuaranteesAxis_loc")
 *             .plusAttribute(q"xlink:to", "jenv-bw2-dim_BW2_2019-01-01_383e_ref")
 *             .plusAttribute(q"xlink:type", "arc")
 *         }
 *         .plusChild {
 *           emptyElem(q"link:referenceArc")
 *             .withNeededExtraScope(Set(q"xlink:type"), scope.filterKeysCompatibly(Set("xlink")))
 *             .plusAttribute(q"xlink:arcrole", "http://www.xbrl.org/2003/arcrole/concept-reference")
 *             .plusAttribute(q"xlink:from", "jenv-bw2-dim_ReceivablesOtherRelatedPartiesCurrentAxis_loc")
 *             .plusAttribute(q"xlink:to", "jenv-bw2-dim_RJ_2019-01-01_115_214_ref")
 *             .plusAttribute(q"xlink:type", "arc")
 *         }
 *     }
 *     .ensuring(_.findAllDescendantElemsOrSelf.map(_.stableScope).distinct.sizeIs > 1)
 *     .ensuring(_.combinedStableScope == scope)
 *     .withoutNamespaceUndeclarations
 *     .ensuring(_.findAllDescendantElemsOrSelf.map(_.stableScope).distinct.sizeIs > 1)
 *     .ensuring(_.combinedStableScope == scope)
 *     .havingSameScopeInDescendantsOrSelf
 *     .ensuring(_.findAllDescendantElemsOrSelf.map(_.stableScope).distinct.sizeIs == 1)
 *     .ensuring(_.combinedStableScope == scope)
 *
 *   nodebuilder.Document(Some(new URI("http://bogus-host/bogus-uri/bogus.xml")), elm)
 * }
 * }}}
 *
 * The element creation API based on [[eu.cdevreeze.yaidom2.node.nodebuilder.NodeBuilders.Elem]], which in turn is based on
 * [[eu.cdevreeze.yaidom2.core.StableScope]], is backed by a small "mathematical theory". This theory shows for example that
 * each such element can have all namespace declarations at the root level, without exception.
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
}
