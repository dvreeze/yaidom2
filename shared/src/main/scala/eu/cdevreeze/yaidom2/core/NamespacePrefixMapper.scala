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

import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable.SeqMap

/**
 * Mapper from namespaces to prefixes. All prefixes are non-empty strings, so the "default namespace" cannot be expressed in this
 * mapping.
 *
 * Unlike Scopes, namespace prefix mappings can map multiple namespaces to the same prefix. Namespace prefix mappings are not
 * used for holding an element's in-scope namespaces (that's what Scope and PrefixedScope do), but they are used "in the background"
 * to generate prefixes from namespaces.
 *
 * When using a NamespacePrefixMapper in the background in an element creation DSL, it works best if within the same XML
 * document (to create with the DSL) all used namespaces point to different prefixes. Typically that should indeed be the case.
 * More generally, the NamespacePrefixMapper works best in a setting where namespace prefixes are chosen/used in a disciplined way.
 *
 * Namespace prefix mappers are encouraged to consistently return the same (optional) prefix for each individual namespace.
 *
 * Note that sometimes there are multiple commonly used prefixes for the same well-known namespace, such as "xs" and "xsd" for the
 * namespace of XML Schema. Obviously a NamespacePrefixMapper should make a choice of which prefix to link to the namespace in such cases.
 *
 * @author Chris de Vreeze
 */
trait NamespacePrefixMapper {

  /**
   * Finds the optional non-empty namespace prefix for the given namespace.
   */
  def findPrefix(namespace: String): Option[String]

  /**
   * Returns the equivalent of `findPrefix(namespace).get`.
   */
  final def getPrefix(namespace: String): String = {
    findPrefix(namespace).getOrElse(sys.error(s"No prefix found for namespace '$namespace'"))
  }
}

object NamespacePrefixMapper {

  /**
   * NamespacePrefixMapper backed by a Map from namespaces to prefixes. Multiple namespaces may map to the same prefix
   * (although this is typically not recommended), but each namespace has at most one (non-empty) prefix linked to it.
   */
  final case class NamespacePrefixMap(mappings: Map[String, String]) extends NamespacePrefixMapper {
    require(mappings.values.forall(_.nonEmpty), s"Empty prefix not allowed, but got mappings '$mappings'")

    def findPrefix(namespace: String): Option[String] = mappings.get(namespace)

    /**
     * Returns true if all namespaces map to unique prefixes, that is, if the inverse Map from prefixes to unique namespaces exists.
     */
    def isInvertible: Boolean = {
      mappings.values.toSeq.distinct.size == mappings.keySet.size
    }
  }

  /**
   * Layered NamespacePrefixMapper, repeatedly falling back to the next layer if no mapping has been found.
   */
  final class LayeredNamespacePrefixMapper(val namespacePrefixMappings: Seq[NamespacePrefixMapper]) extends NamespacePrefixMapper {

    def findPrefix(namespace: String): Option[String] = {
      namespacePrefixMappings.view.flatMap(_.findPrefix(namespace)).headOption.ensuring(_.forall(_.nonEmpty))
    }
  }

  object LayeredNamespacePrefixMapper {

    def apply(namespacePrefixMappings: Seq[NamespacePrefixMapper]): LayeredNamespacePrefixMapper = {
      new LayeredNamespacePrefixMapper(namespacePrefixMappings)
    }
  }

  /**
   * NamespacePrefixMapper backed by a cache from namespaces to prefixes. The cache only grows, so it is not a real cache,
   * because it lacks an eviction strategy. Prefixes are generated on cache misses, using the provided function, that takes
   * the last prefix (if any) as function argument. The last prefix, if any, should be the one of the mapping added last
   * to the SeqMap of mappings.
   *
   * Note that the absence of a cache eviction strategy should not really be a problem in practice, since typical usage
   * scenarios do not deal with many thousands of namespaces. Of course this is only true if these prefix generators (or
   * at least their state) are rather global program state.
   */
  final class CachingPrefixGenerator(
    val mappings: AtomicReference[SeqMap[String, String]],
    val getNextPrefix: Option[String] => String) extends NamespacePrefixMapper {

    def findPrefix(namespace: String): Option[String] = {
      val prefix: String = mappings.updateAndGet { currMappings: SeqMap[String, String] =>
        if (currMappings.contains(namespace)) {
          currMappings
        } else {
          val nextPrefix: String = getNextPrefix(currMappings.lastOption.map(_._2))
          currMappings.updated(namespace, nextPrefix)
        }
      }.apply(namespace).ensuring(_.nonEmpty)

      Some(prefix)
    }
  }

  object CachingPrefixGenerator {

    /**
     * Default function to generate a prefix from the optional last prefix. The prefixes generated by this function
     * typically do not clash with well-known prefixes using in common XML dialects. The format of the generated
     * prefixes is the string "ns" followed by a positive integer number.
     */
    def getNextPrefix(lastPrefixOption: Option[String]): String = {
      lastPrefixOption match {
        case None => "ns1"
        case Some(s) =>
          require(s.lastOption.exists(_.isDigit), s"The last prefix is not a prefix ending in a digit: '$s'")
          val (nonDigits, digits) = s.span(!_.isDigit)
          s"$nonDigits${digits.ensuring(_.nonEmpty).toInt + 1}"
      }
    }

    def newInstance(currentMappings: SeqMap[String, String]): CachingPrefixGenerator = {
      new CachingPrefixGenerator(new AtomicReference(currentMappings), getNextPrefix)
    }

    def newInstance(): CachingPrefixGenerator = newInstance(SeqMap.empty)
  }

  /**
   * Returns a NamespacePrefixMapper backed by a Map from namespaces to prefixes.
   */
  def fromMap(mappings: Map[String, String]): NamespacePrefixMap = NamespacePrefixMap(mappings)

  /**
   * Returns a layered NamespacePrefixMapper, that repeatedly falls back to the next layer if no mapping has been found.
   */
  def layering(namespacePrefixMappings: Seq[NamespacePrefixMapper]): LayeredNamespacePrefixMapper = {
    LayeredNamespacePrefixMapper(namespacePrefixMappings)
  }

  /**
   * Returns a CachingPrefixGenerator using the passed mappings as start state.
   */
  def fallback(currentMappings: SeqMap[String, String]): CachingPrefixGenerator = {
    CachingPrefixGenerator.newInstance(currentMappings)
  }

  /**
   * Returns a fresh CachingPrefixGenerator.
   */
  def fallback(): CachingPrefixGenerator = fallback(SeqMap.empty)

  /**
   * Returns `layering(Seq(fromMap(mappings), fallback()))`. This NamespacePrefixMapper will never fail to return a prefix.
   * This is typically a good default NamespacePrefixMapper, provided the passed mappings are almost complete, and multiple
   * namespaces map to unique prefixes.
   */
  def fromMapWithFallback(mappings: Map[String, String]): NamespacePrefixMapper = {
    layering(Seq(fromMap(mappings), fallback()))
  }
}
