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
import java.io.FileInputStream
import java.net.URI

import eu.cdevreeze.yaidom2.node.saxon
import eu.cdevreeze.yaidom2.node.saxon.SaxonProducers
import eu.cdevreeze.yaidom2.queryapi.oo.BackingDocumentApi
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxo
import eu.cdevreeze.yaidom2.sampleapp.rewritexbrl.taxorewriter.TaxonomyTransformer
import javax.xml.transform.stream.StreamSource
import net.sf.saxon.s9api.Processor
import net.sf.saxon.s9api.Serializer

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
    val docMap = files.map(f => getOriginalUri(f, inputDir) -> parseFile(f, inputDir)).toMap

    val inputTaxonomy = new taxo.Taxonomy(Set.empty, docMap)

    val taxonomyTransformer = new TaxonomyTransformer(inputTaxonomy)

    println(s"Transforming ${docMap.size} taxonomy XML files ...")
    val outputTaxonomy = taxonomyTransformer.transformTaxonomy()

    println("Ready transforming " + outputTaxonomy.documentMap.size + " files. Writing them to file system ...")

    outputTaxonomy.documentMap.values.foreach(d => serializeDocument(d.doc, outputDir))

    println("Ready")
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

  private def parseFile(file: File, rootDir: File): taxo.TaxonomyDocument = {
    val docBuilder = processor.newDocumentBuilder()
    val originalUri = getOriginalUri(file, rootDir)
    val doc = docBuilder.build(new StreamSource(new FileInputStream(file), originalUri.toString))
    taxo.TaxonomyDocument.build(saxon.Document(doc))
  }

  private def getOriginalUri(file: File, rootDir: File): URI = {
    catalog(rootDir).toSeq.sortBy(_._2.toString.length).reverse
      .find(kv => file.toURI.toString.startsWith(kv._2.toString))
      .map(kv => kv._1.resolve(kv._2.relativize(file.toURI)))
      .getOrElse(file.toURI)
  }

  private def catalog(rootDir: File): Map[URI, URI] = {
    require(rootDir.isDirectory)

    // Very sensitive and minimal!
    Map(
      URI.create("http://www.nltaxonomie.nl/") -> rootDir.toURI.resolve("www.nltaxonomie.nl/"),
      URI.create("http://www.xbrl.org/") -> rootDir.toURI.resolve("www.xbrl.org/"),
      URI.create("http://www.w3.org/") -> rootDir.toURI.resolve("www.w3.org/"),
    )
  }

  private def getFileUri(originalUri: URI, rootDir: File): URI = {
    catalog(rootDir).toSeq.sortBy(_._1.toString.length).reverse
      .find(kv => originalUri.toString.startsWith(kv._1.toString))
      .map(kv => kv._2.resolve(kv._1.relativize(originalUri)))
      .getOrElse(originalUri)
  }

  private def serializeDocument(doc: BackingDocumentApi, outputDir: File): Unit = {
    require(outputDir.isDirectory)

    val saxonProducer = new SaxonProducers.DocumentProducer(processor)
    val saxonDoc = saxonProducer.from(doc)

    val fileUri = getFileUri(saxonDoc.docUriOption.getOrElse(URI.create("")), outputDir)
    val outputFile = new File(fileUri)

    val serializer = processor.newSerializer(outputFile)
    serializer.setOutputProperty(Serializer.Property.OMIT_XML_DECLARATION, "no")
    serializer.setOutputProperty(Serializer.Property.INDENT, "yes")

    serializer.serializeXdmValue(saxonDoc.xdmNode)
  }
}
