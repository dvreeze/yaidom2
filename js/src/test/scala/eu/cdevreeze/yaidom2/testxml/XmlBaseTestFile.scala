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

package eu.cdevreeze.yaidom2.testxml

object XmlBaseTestFile {

  val xmlString =
    """<!-- See http://www.w3.org/TR/xmlbase/ -->
      |<doc xml:base="http://example.org/today/"
      |     xmlns:xlink="http://www.w3.org/1999/xlink">
      |    <head>
      |        <title>Virtual Library</title>
      |    </head>
      |    <body>
      |        <paragraph>See <link xlink:type="simple" xlink:href="new.xml">what's
      |            new</link>!</paragraph>
      |        <paragraph>Check out the hot picks of the day!</paragraph>
      |        <olist xml:base="/hotpicks/">
      |            <item>
      |                <link xlink:type="simple" xlink:href="pick1.xml">Hot Pick #1</link>
      |            </item>
      |            <item>
      |                <link xlink:type="simple" xlink:href="pick2.xml">Hot Pick #2</link>
      |            </item>
      |            <item>
      |                <link xlink:type="simple" xlink:href="pick3.xml">Hot Pick #3</link>
      |            </item>
      |        </olist>
      |    </body>
      |</doc>
      |""".stripMargin
}
