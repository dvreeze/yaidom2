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

package eu.cdevreeze.yaidom2.testconsole

import java.io.File

import eu.cdevreeze.yaidom2.core.EName
import eu.cdevreeze.yaidom2.node.saxon.SaxonNodes
import eu.cdevreeze.yaidom2.queryapi.oo.steps.ElemSteps._
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.streams.Predicates._
import net.sf.saxon.s9api.streams.Steps._

/**
 * Printer of some element info about a parsed XML file. One use case is to parse a large XML file and print
 * the number of elements, etc., getting a feel for the structure and size of the XML DOM tree.
 *
 * This program also helps in finding out how fast yaidom2 parsing and querying is for large XML input files, without
 * automated tests that do the same but require a lot of memory.
 *
 * @author Chris de Vreeze
 */
object XmlElementInfoPrinter {

  def main(args: Array[String]): Unit = {
    require(args.length == 1, s"Usage: XmlElementInfoParser <XML file path>")

    val xmlFile = new File(args(0))

    val processor = new Processor(false)

    println(s"Parsing file '$xmlFile'")

    val docBuilder = processor.newDocumentBuilder()
    val underlyingDoc = docBuilder.build(xmlFile)

    println(s"Ready parsing file")

    val docElem = SaxonNodes.Elem(underlyingDoc.select(child(isElement)).findFirst().get)

    val elemCount = docElem.select(descendantElemsOrSelf()).size

    println(s"Element count: $elemCount")

    val elemNameCounts: Map[EName, Int] =
      docElem.select(descendantElemsOrSelf()).groupBy(_.name).view.mapValues(_.size).toMap

    val showCount = 30
    println(s"Element name counts (first $showCount at most): ${elemNameCounts.toSeq.sortBy(_._2).reverse.take(showCount)}")

    println()
    println("Element ancestries:")
    println()

    val elemNames: Seq[EName] = elemNameCounts.keySet.toSeq.sortBy(_.toString)

    elemNames.foreach { elemName =>
      val ancestries: Seq[Seq[SaxonNodes.Elem]] =
        docElem.select(descendantElemsOrSelf(elemName)).map(_.select(ancestorElemsOrSelf()))

      val ancestryCounts: Map[Seq[EName], Int] =
        ancestries.groupBy(_.map(_.name)).view.mapValues(_.size).toMap

      ancestryCounts.toSeq.foreach { case (ancestry, cnt) =>
        val filler: String = " " * (10 - cnt.toString.length)

        println(s"Count: $cnt." + filler + ancestry.mkString(", "))
      }
    }
  }
}
