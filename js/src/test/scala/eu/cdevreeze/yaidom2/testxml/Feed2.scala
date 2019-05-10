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

object Feed2 {

  val xmlString =
    """<feed xmlns="http://www.w3.org/2005/Atom">
      |
      |    <title>Example Feed</title>
      |    <rights type="xhtml"
      |            example:type="silly"
      |            xmlns:example="http://xmlportfolio.com/xmlguild-examples">
      |        <div xmlns="http://www.w3.org/1999/xhtml">
      |            You may not read, utter, interpret, or otherwise
      |            <strong>verbally process</strong> the words
      |            contained in this feed without <em>express written
      |            permission</em> from the authors.
      |        </div>
      |    </rights>
      |
      |    <!-- ... -->
      |
      |</feed>""".stripMargin
}
