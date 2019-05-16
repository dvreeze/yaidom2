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

package eu.cdevreeze.yaidom2.sampleapp.rewritexbrl

object Namespaces {

  /** Namespace of xml:base, xml:lang etc. */
  val XmlNamespace = "http://www.w3.org/XML/1998/namespace"

  val XsNamespace = "http://www.w3.org/2001/XMLSchema"
  val XsiNamespace = "http://www.w3.org/2001/XMLSchema-instance"
  val XLinkNamespace = "http://www.w3.org/1999/xlink"
  val LinkNamespace = "http://www.xbrl.org/2003/linkbase"
  val XbrliNamespace = "http://www.xbrl.org/2003/instance"
  val XbrldtNamespace = "http://xbrl.org/2005/xbrldt"
  val GenNamespace = "http://xbrl.org/2008/generic"
  val LabelNamespace = "http://xbrl.org/2008/label"
  val ReferenceNamespace = "http://xbrl.org/2008/reference"
  val RefNamespace = "http://www.xbrl.org/2006/ref"
  val VariableNamespace = "http://xbrl.org/2008/variable"
  val FormulaNamespace = "http://xbrl.org/2008/formula"
  val ValidationNamespace = "http://xbrl.org/2008/validation"
  val InstancesNamespace = "http://xbrl.org/2010/variable/instance"
  val TableNamespace = "http://xbrl.org/2014/table"
  val MsgNamespace = "http://xbrl.org/2010/message"
  val SevNamespace = "http://xbrl.org/2016/assertion-severity"
  val SeveNamespace = "http://xbrl.org/2016/assertion-severity/error"
  val VaNamespace = "http://xbrl.org/2008/assertion/value"
  val EaNamespace = "http://xbrl.org/2008/assertion/existence"
  val CaNamespace = "http://xbrl.org/2008/assertion/consistency"
  val CfNamespace = "http://xbrl.org/2008/filter/concept"
  val BfNamespace = "http://xbrl.org/2008/filter/boolean"
  val DfNamespace = "http://xbrl.org/2008/filter/dimension"
  val EfNamespace = "http://xbrl.org/2008/filter/entity"
  val GfNamespace = "http://xbrl.org/2008/filter/general"
  val MfNamespace = "http://xbrl.org/2008/filter/match"
  val PfNamespace = "http://xbrl.org/2008/filter/period"
  val RfNamespace = "http://xbrl.org/2008/filter/relative"
  val SsfNamespace = "http://xbrl.org/2008/filter/segment-scenario"
  val TfNamespace = "http://xbrl.org/2008/filter/tuple"
  val UfNamespace = "http://xbrl.org/2008/filter/unit"
  val VfNamespace = "http://xbrl.org/2008/filter/value"
  val AcfNamespace = "http://xbrl.org/2010/filter/aspect-cover"
  val CrfNamespace = "http://xbrl.org/2010/filter/concept-relation"
  val GplNamespace = "http://xbrl.org/2013/preferred-label"
  val XfiNamespace = "http://www.xbrl.org/2008/function/instance"
  val CfiNamespace = "http://xbrl.org/2010/custom-function"

  val FormulaFilterNamespaces =
    Set(
      CfNamespace,
      BfNamespace,
      DfNamespace,
      EfNamespace,
      GfNamespace,
      MfNamespace,
      PfNamespace,
      RfNamespace,
      SsfNamespace,
      TfNamespace,
      UfNamespace,
      VfNamespace,
      AcfNamespace,
      CrfNamespace)

  // Namespaces in the locator-free model. The locator-free taxonomy model is not XBRL (although very similar-looking),
  // but it does use the XLink schemas with namespaces "http://www.w3.org/1999/xlink" and "http://www.xbrl.org/2003/XLink".
  // The former schema declares XLink attributes, and the latter schema declares substitution groups for XLink elements
  // in an XBRL context.

  // We need an extra schema and namespace for locator-free extended links. It introduces a substitution group for
  // extended links (themselves in substitution group xl:extended) that restrict the content model by not allowing
  // any XLink locators. The aim is to prohibit XLink locators and simple links in locator-free taxonomies.

  // The "C" prefixing the namespace prefixes stands for concise. It is not the case that the taxonomy files are concise,
  // but the set of taxononmy files needed for certain validation tasks is typically more concise than entire DTSes.

  // The "CLink" namespace mirrors most of the "link" namespace, but without the "loc" element. The linkbase element
  // is also mirrored, and the differing namespace is indeed not an XBRL linkbase meant to be understood by XBRL tooling.
  // Some differences with the original "link" namespace and schema: of course we restrict the extended links to counterparts
  // having no locators, we have no loc element, no footnotes, no roleRefs/arcroleRefs/schemaRefs/linkbaseRefs (so also
  // not in our linkbase elements). We do re-use the "link namespace" roleTypes/arcroleTypes and their content, however.

  val CLinkNamespace: String = "http://www.locfreexbrl.org/2003/linkbase" // prefix "clink"

  // Note that the "instance" schema, which also contains item types, also depends on the "link" schema, so probably it is hard
  // to get rid of that schema, and likely we do not have to, as long as we use our linkbase elements with their defined content.
  // We can define a restricted schema for the "link" namespace, in the same way that the Taxonomy Packages spec restricts catalogs.

  // Without the link:loc element or any direct counterpart, we need an extensible scheme for taxonomy element keys.
  // The (new) substitution group for those keys is taxonomyElementKey. The most common element in that substitution
  // group is conceptKey.

  val CKeyNamespace: String = "http://www.locfreexbrl.org/2019/key" // prefix "ckey"

  // Since we have to mirror all extended links, limiting them to content without any locators, we need to do the same
  // for generic links. See the following namespace. Fortunately, we can still use generic arcs and any XLink resource
  // (including tables, assertions etc.) in the locator-free taxonomies.

  val CGenNamespace: String = "http://www.locfreexbrl.org/2008/generic" // prefix "cgen"

  // Namespace for the counterpart of xbrldt:typedDomainRef attributes

  val CXbrldtNamespace: String = "http://locfreexbrl.org/2005/xbrldt"
}
