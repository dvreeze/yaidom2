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
 * Qualified name. See http://www.w3.org/TR/xml-names11/.
 * It is the combination of an optional prefix and a mandatory local part. It is not like a `QName` in Java, which
 * is more like what yaidom calls an expanded name.
 *
 * @author Chris de Vreeze
 */
sealed trait QName {

  def localPart: String

  def prefixOption: Option[String]
}

final case class UnprefixedName(localPart: String) extends QName {
  require(localPart ne null) // scalastyle:off null

  override def prefixOption: Option[String] = None

  /** The `String` representation as it appears in XML, that is, the localPart */
  override def toString: String = localPart
}

final case class PrefixedName(prefix: String, localPart: String) extends QName {
  require(prefix ne null) // scalastyle:off null
  require(localPart ne null) // scalastyle:off null

  override def prefixOption: Option[String] = Some(prefix)

  /** The `String` representation as it appears in XML. For example, <code>xs:schema</code> */
  override def toString: String = s"$prefix:$localPart"
}

object QName {

  /** Creates a `QName` from an optional prefix and a localPart */
  def apply(prefixOption: Option[String], localPart: String): QName = {
    prefixOption.map(pref => PrefixedName(pref, localPart)).getOrElse(UnprefixedName(localPart))
  }

  /** Creates a `PrefixedName` from a prefix and a localPart */
  def apply(prefix: String, localPart: String): QName = PrefixedName(prefix, localPart)

  /** Creates an `UnprefixedName` from a localPart */
  def fromLocalName(localPart: String): QName = UnprefixedName(localPart)

  /**
   * Parses a `String` into a `QName`. The `String` (after trimming) must conform to the `toString` format of
   * a `PrefixedName` or `UnprefixedName`.
   */
  def parse(s: String): QName = {
    val st = s.trim

    val arr = st.split(':')
    require(arr.length <= 2, s"Expected at most 1 colon in QName '$st'")

    arr.length match {
      case 1 => UnprefixedName(st)
      case 2 => PrefixedName(arr(0), arr(1))
      case _ => sys.error(s"Did not expect more than 1 colon in QName '$st'")
    }
  }

  /**
   * Extractor turning a QName into a pair of an optional prefix, and a local part.
   *
   * With this extractor one can pattern match on arbitrary QNames, and not just on prefixed or unprefixed names.
   */
  def unapply(qname: QName): Option[(Option[String], String)] = qname match {
    case UnprefixedName(localPart) => Some((None, localPart))
    case PrefixedName(prefix, localPart) => Some((Some(prefix), localPart))
    case null => None
  }
}
