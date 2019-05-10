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

object Cars {

  val xmlString =
    """<!-- Copied from http://groovy.codehaus.org/Reading+XML+using+Groovy%27s+XmlParser -->
      |<records>
      |    <car name='HSV Maloo' make='Holden' year='2006'>
      |        <country>Australia</country>
      |        <record type='speed'>Production Pickup Truck with speed of 271kph
      |        </record>
      |    </car>
      |    <car name='P50' make='Peel' year='1962'>
      |        <country>Isle of Man</country>
      |        <record type='size'>Smallest Street-Legal Car at 99cm wide and 59 kg
      |            in weight</record>
      |    </car>
      |    <car name='Royale' make='Bugatti' year='1931'>
      |        <country>France</country>
      |        <record type='price'>Most Valuable Car at $15 million</record>
      |    </car>
      |</records>""".stripMargin
}
