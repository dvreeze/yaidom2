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

package eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.console

import java.io.File

import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxo
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxorewriter.TaxonomyTransformer
import net.sf.saxon.s9api.Processor

object TransformTaxonomy {

  private val processor = new Processor(false)

  def main(args: Array[String]): Unit = {
    require(args.length == 2, s"Usage: TransformTaxonomy <input dir> <output dir>")

    val inputDir = new File(args(0)).ensuring(_.isDirectory)
    val outputDir = new File(args(1))
    outputDir.mkdirs()
    require(outputDir.isDirectory)

    println(s"Reading files ...")
    val files = readFiles(inputDir, filterFile)

    println(s"Parsing ${files.size} XML files ...")
    val docMap = files.map(f => f.toURI -> parseFile(f)).toMap

    val inputTaxonomy = new taxo.Taxonomy(Set.empty, docMap)

    val taxonomyTransformer = new TaxonomyTransformer(inputTaxonomy)

    println(s"Transforming ${docMap.size} taxonomy XML files ...")
    val outputTaxonomy = taxonomyTransformer.transformTaxonomy()

    println("Ready transforming " + outputTaxonomy.documentMap.size + " files")
  }

  private def filterFile(f: File): Boolean = {
    f.isFile && Set(".xml", ".xsd").exists(ext => f.getName().endsWith(ext))
  }

  private def readFiles(dir: File, fileFilter: File => Boolean): Seq[File] = {
    dir.listFiles.toSeq.flatMap { f =>
      if (f.isDirectory) {
        // Recursive call
        readFiles(f, fileFilter)
      } else {
        if (f.isFile && fileFilter(f)) {
          Seq(f)
        } else {
          Seq.empty
        }
      }
    }
  }

  private def parseFile(file: File): taxo.TaxonomyDocument = {
    val docBuilder = processor.newDocumentBuilder()
    val doc = docBuilder.build(file)
    taxo.TaxonomyDocument.build(saxon.Document(doc))
  }
}
