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

object MiniXmlBaseTestFile {

  val xmlString =
    """<!-- See http://www.w3.org/TR/xmlbase/ -->
      |<e1 xml:base="http://example.org/wine/">
      |    <!-- Adapted 4th character in next xml:base to circumvent issue with (internal) URI parsing/resolution in JDK -->
      |    <e2 xml:base="rose">ros&#x00E9;</e2>
      |</e1>
      |""".stripMargin
}
