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

package eu.cdevreeze.yaidom2.node.simple

import scala.collection.immutable.SeqMap

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.core.QName
import eu.cdevreeze.yaidom2.core.Scope
import eu.cdevreeze.yaidom2.creationapi.ElemCreationApi

/**
 * Element creation API for simple elements. This class is still experimental.
 *
 * @author Chris de Vreeze
 */
private[yaidom2] final case class SimpleElemCreator(scope: Scope, mayDropDefaultNamespace: Boolean) extends ElemCreationApi {

  import SimpleElemCreator._

  type NodeType = SimpleNodes.Node

  type ElemType = SimpleNodes.Elem

  def droppingDefaultNamespaceIfNeeded(dropDefaultNs: Boolean): SimpleElemCreator = {
    SimpleElemCreator(scope, dropDefaultNs)
  }

  def elem(name: EName, children: Seq[NodeType]): ElemType = {
    val ScopedQName(qname, newScope) = getScopedElementQName(name)

    new Elem(qname, SeqMap.empty, newScope, children.to(Vector))
  }

  def elem(name: EName, attributes: SeqMap[EName, String], children: Seq[NodeType]): ElemType = {
    val ScopedQName(qname, newScope) = getScopedElementQName(name)
    val attributesByQName: SeqMap[QName, String] = attributes.toSeq.map(kv => getAttributeQName(kv._1) -> kv._2).to(SeqMap)

    new Elem(qname, attributesByQName, newScope, children.to(Vector))
  }

  def textElem(name: EName, txt: String): ElemType = {
    val ScopedQName(qname, newScope) = getScopedElementQName(name)

    new Elem(qname, SeqMap.empty, newScope, Vector(Text(txt, isCData = false)))
  }

  def textElem(name: EName, attributes: SeqMap[EName, String], txt: String): ElemType = {
    val ScopedQName(qname, newScope) = getScopedElementQName(name)
    val attributesByQName: SeqMap[QName, String] = attributes.toSeq.map(kv => getAttributeQName(kv._1) -> kv._2).to(SeqMap)

    new Elem(qname, attributesByQName, newScope, Vector(Text(txt, isCData = false)))
  }

  def emptyElem(name: EName): ElemType = {
    val ScopedQName(qname, newScope) = getScopedElementQName(name)

    new Elem(qname, SeqMap.empty, newScope, Vector.empty)
  }

  def emptyElem(name: EName, attributes: SeqMap[EName, String]): ElemType = {
    val ScopedQName(qname, newScope) = getScopedElementQName(name)
    val attributesByQName: SeqMap[QName, String] = attributes.toSeq.map(kv => getAttributeQName(kv._1) -> kv._2).to(SeqMap)

    new Elem(qname, attributesByQName, newScope, Vector.empty)
  }

  private def getScopedElementQName(ename: EName): ScopedQName = {
    findScopedQName(ename, scope)
      .getOrElse(sys.error(s"Could not turn element name $ename into a QName (scope $scope, possibly leaving out the default namespace, if any)"))
  }

  private def getAttributeQName(ename: EName): QName = {
    findScopedQName(ename, scope.withoutDefaultNamespace)
      .getOrElse(sys.error(s"Could not turn attribute name $ename into a QName (scope ${scope.withoutDefaultNamespace})"))
      .qname
  }

  /**
   * Finds the optional QName for the given EName, using the given Scope. Also returns the Scope that may be the same
   * as the input Scope, or may be the same except for the removal of the default namespace.
   */
  private def findScopedQName(ename: EName, sc: Scope): Option[ScopedQName] = {
    if (ename.namespaceUriOption.isEmpty) {
      if (sc.defaultNamespaceOption.isEmpty) {
        Some(ScopedQName(QName.fromLocalName(ename.localPart), sc))
      } else {
        if (mayDropDefaultNamespace) {
          Some(ScopedQName(QName.fromLocalName(ename.localPart), sc.withoutDefaultNamespace))
        } else {
          None
        }
      }
    } else {
      if (ename.namespaceUriOption.contains(Scope.XmlNamespace)) {
        Some(ScopedQName(QName("xml", ename.localPart), sc))
      } else {
        // Below the empty string is also treated as a prefix
        val prefixes: Set[String] = sc.prefixesForNamespace(ename.namespaceUriOption.get)
        val prefixesWithoutEmptyPrefix: Set[String] = prefixes.filterNot(_.isEmpty)

        val prefixOption: Option[String] = prefixesWithoutEmptyPrefix.headOption.orElse(prefixes.headOption)

        prefixOption.map { prefix =>
          if (prefix.isEmpty) {
            ScopedQName(QName.fromLocalName(ename.localPart), sc)
          } else {
            ScopedQName(QName(prefix, ename.localPart), sc)
          }
        }
      }
    }.ensuring(_.forall(_.scope.withoutDefaultNamespace == sc.withoutDefaultNamespace))
      .ensuring(_.forall(scopedQn => scopedQn.scope.resolveQNameOption(scopedQn.qname).contains(ename)))
  }
}

private[yaidom2] object SimpleElemCreator {

  private case class ScopedQName(qname: QName, scope: Scope)

  def strict(scope: Scope): SimpleElemCreator = SimpleElemCreator(scope, mayDropDefaultNamespace = false)

  def lenient(scope: Scope): SimpleElemCreator = SimpleElemCreator(scope, mayDropDefaultNamespace = true)
}
