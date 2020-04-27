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

import scala.collection.immutable.ListMap

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

  object NamespacePrefixMap {

    /**
     * Creates a NamespacePrefixMap from a Map from prefixes to namespaces. Note that prefixes may get lost this way.
     * If there are multiple prefixes for the same namespaces, the last prefix in insertion order wins. Note that multiple
     * namespaces for the same prefix are not possible, because the input is a Map from prefixes to namespaces.
     *
     * That is, the resulting NamespacePrefixMap has namespace-prefix mappings
     * `prefixNamespaceMap.toSeq.map(_.swap).reverse.distinctBy(_._1).reverse.toMap`.
     */
    def fromPrefixToNamespaceMap(prefixNamespaceMap: ListMap[String, String]): NamespacePrefixMap = {
      val mappings: Map[String, String] = prefixNamespaceMap.toSeq.map(_.swap).reverse.distinctBy(_._1).reverse.toMap

      NamespacePrefixMap(mappings)
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
   * because it lacks an eviction strategy. Prefixes are generated on cache misses, using the provided prefix start string,
   * and appending an (increasing) integer to it. The last prefix, if any, should be the one of the mapping added last to the
   * SeqMap of mappings.
   *
   * Note that the absence of a cache eviction strategy should not really be a problem in practice, since typical usage
   * scenarios do not deal with many thousands of namespaces. Of course this is only true if these prefix generators (or
   * at least their state) are rather global program state.
   */
  final class CachingPrefixGenerator private (val mappings: AtomicReference[ListMap[String, String]], val prefixWithoutSeqNr: String)
      extends NamespacePrefixMapper {

    require(prefixWithoutSeqNr.nonEmpty, s"Empty prefix after removing sequence number not allowed")

    def findPrefix(namespace: String): Option[String] = {
      val prefix: String = mappings
        .updateAndGet { currMappings: ListMap[String, String] =>
          if (currMappings.contains(namespace)) {
            currMappings
          } else {
            val nextPrefix: String = getNextPrefix(currMappings)
            currMappings.updated(namespace, nextPrefix)
          }
        }
        .apply(namespace)
        .ensuring(_.nonEmpty)

      Some(prefix)
    }

    private def getNextPrefix(currMappings: ListMap[String, String]): String = {
      val lastPrefixOption: Option[String] = currMappings.lastOption.map(_._2)

      val lastSeqNrOption: Option[Int] = lastPrefixOption.map { s =>
        require(s.lastOption.exists(_.isDigit), s"The prefix does not end in a digit: '$s'")
        s.dropWhile(!_.isDigit).toInt
      }

      val nextSeqNr: Int = lastSeqNrOption.getOrElse(0) + 1
      s"$prefixWithoutSeqNr$nextSeqNr"
    }
  }

  object CachingPrefixGenerator {

    def newInstance(currentMappings: ListMap[String, String], prefixStartString: String): CachingPrefixGenerator = {
      new CachingPrefixGenerator(new AtomicReference(currentMappings), prefixStartString)
    }

    def newInstance(prefixStartString: String): CachingPrefixGenerator = newInstance(ListMap.empty, prefixStartString)

    def newInstance(): CachingPrefixGenerator = newInstance("ns")
  }

  /**
   * Returns a NamespacePrefixMapper backed by a Map from namespaces to prefixes.
   */
  def fromMap(mappings: Map[String, String]): NamespacePrefixMap = NamespacePrefixMap(mappings)

  /**
   * Returns a NamespacePrefixMapper from a Map from prefixes to namespaces. That is, returns
   * `NamespacePrefixMap.fromPrefixToNamespaceMap(prefixNamespaceMap)`.
   */
  def fromPrefixToNamespaceMap(prefixNamespaceMap: ListMap[String, String]): NamespacePrefixMap = {
    NamespacePrefixMap.fromPrefixToNamespaceMap(prefixNamespaceMap)
  }

  /**
   * Returns a layered NamespacePrefixMapper, that repeatedly falls back to the next layer if no mapping has been found.
   */
  def layering(namespacePrefixMappings: Seq[NamespacePrefixMapper]): LayeredNamespacePrefixMapper = {
    LayeredNamespacePrefixMapper(namespacePrefixMappings)
  }

  /**
   * Returns a CachingPrefixGenerator using the passed mappings as start state.
   */
  def fallback(currentMappings: ListMap[String, String]): CachingPrefixGenerator = {
    CachingPrefixGenerator.newInstance(currentMappings, "ns")
  }

  /**
   * Returns a fresh CachingPrefixGenerator.
   */
  def fallback(): CachingPrefixGenerator = fallback(ListMap.empty)

  /**
   * Returns `layering(Seq(fromMap(mappings), fallback()))`. This NamespacePrefixMapper will never fail to return a prefix.
   * This is typically a good default NamespacePrefixMapper, provided the passed mappings are almost complete, and multiple
   * namespaces map to unique prefixes.
   */
  def fromMapWithFallback(mappings: Map[String, String]): NamespacePrefixMapper = {
    layering(Seq(fromMap(mappings), fallback()))
  }

  /**
   * Returns `layering(Seq(fromPrefixToNamespaceMap(prefixNamespaceMap), fallback()))`.
   */
  def fromPrefixToNamespaceMapWithFallback(prefixNamespaceMap: ListMap[String, String]): NamespacePrefixMapper = {
    layering(Seq(fromPrefixToNamespaceMap(prefixNamespaceMap), fallback()))
  }
}
