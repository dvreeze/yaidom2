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

package eu.cdevreeze.yaidom2.core

/**
 * Expanded name. See http://www.w3.org/TR/xml-names11/. It has a localPart and an optional namespace URI.
 * Semantically like a `QName` in Java, but not keeping the prefix.
 *
 * @author Chris de Vreeze
 */
final case class EName(namespaceUriOption: Option[String], localPart: String) {
  require(namespaceUriOption ne null) // scalastyle:off null
  require(localPart ne null) // scalastyle:off null

  /** The `String` representation, in the format of the `javax.xml.namespace.QName.toString` method */
  override def toString: String = namespaceUriOption match {
    case None => localPart
    case Some(nsUri) =>
      val openBrace = "{"
      val closeBrace = "}"
      s"$openBrace$nsUri$closeBrace$localPart"
  }

  /**
   * Returns the string representation of this EName as URI qualified name, as a braced URI literal followed by
   * an NC-name. See for example https://www.w3.org/TR/xpath-31/#prod-xpath31-URIQualifiedName.
   */
  def toUriQualifiedNameString: String = {
    val ns = namespaceUriOption.getOrElse("")

    s"Q{$ns}$localPart"
  }
}

object EName {

  /** Creates an `EName` from a namespaceUri and a localPart */
  def apply(namespaceUri: String, localPart: String): EName = EName(Some(namespaceUri), localPart)

  /** Creates an `EName` from just a localPart */
  def fromLocalName(localPart: String): EName = EName(None, localPart)

  /**
   * Parses a `String` into an `EName`. The `String` (after trimming) must conform to the `toString` format of an `EName`.
   */
  def parse(s: String): EName = {
    val st = s.trim

    if (st.startsWith("{")) {
      val idx = st.indexOf('}')
      require(idx >= 2 && idx < st.length - 1, s"Opening brace not closed or at incorrect location in EName '${st}'")
      val ns = st.substring(1, idx)
      val localPart = st.substring(idx + 1)
      EName(Some(ns), localPart)
    } else {
      require(st.indexOf("{") < 0, s"No opening brace allowed unless at the beginning in EName '${st}'")
      require(st.indexOf("}") < 0, s"Closing brace without matching opening brace not allowed in EName '${st}'")
      EName(None, st)
    }
  }

  /**
   * Parses a `String` into an `EName`. The `String` must conform to the `toUriQualifiedNameString` format of an `EName`.
   */
  def fromUriQualifiedNameString(uriQualifiedName: String): EName = {
    require(
      uriQualifiedName.startsWith("Q{"),
      s"Not an URI qualified name (it does not start with 'Q{'): $uriQualifiedName")

    val endBraceIdx = uriQualifiedName.indexOf("}")

    require(endBraceIdx > 0, s"Not an URI qualified name (it has no '}'): $uriQualifiedName")

    val ns = uriQualifiedName.substring(2, endBraceIdx)
    val nsOption = if (ns.isEmpty) None else Some(ns)

    val localPart = uriQualifiedName.substring(endBraceIdx + 1)

    require(localPart.length > 0, s"Not an URI qualified name (it has no local part): $uriQualifiedName")

    EName(nsOption, localPart)
  }
}
