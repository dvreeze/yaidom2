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

object SampleXbrlInstance {

  private val xmlStringPart1 =
    """<!-- Created by Charles Hoffman, CPA, 2008-03-27 -->
      |<!-- See http://www.xbrlsite.com/examples/comprehensiveexample/2008-04-18/sample-Instance-Proof.xml. -->
      |<xbrl xmlns='http://www.xbrl.org/2003/instance' xmlns:xbrli='http://www.xbrl.org/2003/instance'
      |      xmlns:link='http://www.xbrl.org/2003/linkbase' xmlns:xlink='http://www.w3.org/1999/xlink'
      |      xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xbrldi="http://xbrl.org/2006/xbrldi"
      |      xmlns:iso4217='http://www.xbrl.org/2003/iso4217' xmlns:gaap='http://xasb.org/gaap'
      |      xmlns:company='http://www.example.com/company' xsi:schemaLocation="">
      |
      |
      |    <!-- Use to turn on/off formula validation. Copy OUTSIDE of CDATA section
      |		to turn on <![CDATA[ ]]> -->
      |
      |
      |    <link:schemaRef xlink:type="simple" xlink:href="gaap.xsd" />
      |
      |    <link:linkbaseRef xlink:type="simple" xlink:href="gaap-formula.xml"
      |                      xlink:arcrole="http://www.w3.org/1999/xlink/properties/linkbase" />
      |
      |
      |    <!-- General Contexts -->
      |    <context id='I-2007'>
      |        <!-- Consolidated Group, Current Period, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='I-2006'>
      |        <!-- Consolodated Group, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='I-2006-OrigionallyReported'>
      |        <!-- Consolodated Group, Prior Period (Origionally Reported), As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsMarch112007Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='I-2005'>
      |        <!-- Consolodated Group, Prior Period Beginning Balances, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2005-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='I-2004'>
      |        <!-- Consolodated Group, Financial Highlights, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2004-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='I-2003'>
      |        <!-- Consolodated Group, Financial Highlights, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2003-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='D-2007'>
      |        <!-- Consolodated Group, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |
      |    <context id='D-2007-PSA'>
      |        <!-- Preferred Stock Class A, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:ClassAPreferredScockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2007-CSA'>
      |        <!-- Common Stock Class A, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:ClassACommonStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |
      |    <context id='D-2006'>
      |        <!-- Consolodated Group, Prior Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2006-01-01</startDate>
      |            <endDate>2006-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2006-PSA'>
      |        <!-- Preferred Stock Class A, Prior Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:ClassAPreferredScockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2006-01-01</startDate>
      |            <endDate>2006-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2006-CSA'>
      |        <!-- Common Stock Class A, Prior Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:ClassACommonStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2006-01-01</startDate>
      |            <endDate>2006-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2005'>
      |        <!-- Consolodated Group, Financial Highlights, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2005-01-01</startDate>
      |            <endDate>2005-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2004'>
      |        <!-- Consolodated Group, Financial Highlights, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2004-01-01</startDate>
      |            <endDate>2004-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2003'>
      |        <!-- Consolodated Group, Financial Highlights, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ConsolidatedGroupDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2003-01-01</startDate>
      |            <endDate>2003-12-31</endDate>
      |        </period>
      |    </context>
      |
      |
      |    <!-- Classes of Preferred Stock -->
      |    <context id="I-2007-PS-All">
      |        <!-- Classes of Preferred Stock, Class A, Current Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:AllClassesOfPreferredStockDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2007-PSA">
      |        <!-- Classes of Preferred Stock, Class A, Current Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:ClassAPreferredScockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-PS-All">
      |        <!-- Classes of Preferred Stock, Class A, Current Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:AllClassesOfPreferredStockDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-PSA">
      |        <!-- Classes of Preferred Stock, Class A, Prior Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:ClassAPreferredScockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2005-PSA">
      |        <!-- Classes of Preferred Stock, Class A, Prior Prior Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:ClassAPreferredScockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2005-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2007-PSB">
      |        <!-- Classes of Preferred Stock, Class B, Current Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:ClassBPreferredStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-PSB">
      |        <!-- Classes of Preferred Stock, Class B, Prior Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfPreferredStockDescriptionAxis">gaap:ClassBPreferredStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |
      |    <!-- Classes of Common Stock -->
      |    <context id="I-2007-CS-All">
      |        <!-- Classes of Common Stock, Class A, Current Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:AllClassesOfCommonStockDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-CS-All">
      |        <!-- Classes of Common Stock, Class A, Current Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:AllClassesOfCommonStockDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2007-CSA">
      |        <!-- Classes of Common Stock, Class A, Current Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:ClassACommonStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-CSA">
      |        <!-- Classes of Common Stock, Class A, Prior Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:ClassACommonStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2005-CSA">
      |        <!-- Classes of Common Stock, Class A, Prior Prior Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:ClassACommonStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2005-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2007-CSB">
      |        <!-- Classes of Common Stock, Class B, Current Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:ClassBCommonStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-CSB">
      |        <!-- Classes of Common Stock, Class B, Prior Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ClassOfCommonStockDescriptionAxis">gaap:ClassBCommonStockMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |
      |
      |""".stripMargin

  private val xmlStringPart2 =
    """    <!-- Leaseholds -->
      |    <context id="D-2007-LIA">
      |        <!-- Leasehold Improvements, Leasehold A, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:LeaseholdAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-LIA">
      |        <!-- Leasehold Improvements, Leasehold A, Current Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:LeaseholdAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-LIA">
      |        <!-- Leasehold Improvements, Leasehold A, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:LeaseholdAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="D-2007-LIB">
      |        <!-- Leasehold Improvements, Leasehold B, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:LeaseholdBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-LIB">
      |        <!-- Leasehold Improvements, Leasehold B, Current Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:LeaseholdBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-LIB">
      |        <!-- Leasehold Improvements, Leasehold B, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:LeaseholdBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="D-2007-LI-ALL">
      |        <!-- Leasehold Improvements, All Leaseholds, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:AllLeaseholdIdentifiersDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-LI-ALL">
      |        <!-- Leasehold Improvements, All Leaseholds, Current Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:AllLeaseholdIdentifiersDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-LI-ALL">
      |        <!-- Leasehold Improvements, All Leaseholds, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingIdentifierAxis">gaap:AllLeaseholdIdentifiersDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:LeaseholdLandAndBuildingStateAxis">gaap:WashingtonMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |
      |
      |    <!-- Debt Instruments -->
      |    <context id="D-2007-DIA">
      |        <!-- Debt Instrument, Instrument A, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-DIA">
      |        <!-- Debt Instrument, Instrument A, Current Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-DIA">
      |        <!-- Debt Instrument, Instrument A, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="D-2007-DIB">
      |        <!-- Debt Instrument, Instrument B, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-DIB">
      |        <!-- Debt Instrument, Instrument B, Current Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-DIB">
      |        <!-- Debt Instrument, Instrument B, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="D-2007-DIC">
      |        <!-- Debt Instrument, Instrument C, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentCMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-DIC">
      |        <!-- Debt Instrument, Instrument C, Current Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentCMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-DIC">
      |        <!-- Debt Instrument, Instrument C, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:DebtInstrumentCMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2007-DI-ALL">
      |        <!-- Debt Instrument, All Debt Instruments, Current Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:AllDebtInstrumentsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-DI-ALL">
      |        <!-- Debt Instrument, All Debt Instruments, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DebtInstrumentIdentifierAxis">gaap:AllDebtInstrumentsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |
      |
      |
      |
      |    <!-- Business Segments -->
      |    <context id='D-2007-ALL'>
      |        <!-- All Operations, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:AllOperationsMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2007-CON'>
      |        <!-- Continuing Operations, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ContinuingOperationsMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2007-E'>
      |        <!-- Consolidating Eliminations, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:EliminationsMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='I-2007-E'>
      |        <!-- Consolidating Eliminations, Current Period, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:EliminationsMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='D-2007-BS1'>
      |        <!-- Business Segment 1, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:BusinessSegmentOneMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2007-BS2'>
      |        <!-- Business Segment 2, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:BusinessSegmentTwoMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='I-2007-BS1'>
      |        <!-- Business Segment 1, Current Period, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:BusinessSegmentOneMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='I-2007-BS2'>
      |        <!-- Business Segment 2, Current Period, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:BusinessSegmentTwoMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='I-2007-CON'>
      |        <!-- Continuing Operations, Current Period, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:ContinuingOperationsMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id='I-2007-ALL'>
      |        <!-- All Operations, Current Period, As of -->
      |        <entity>
      |            <identifier scheme='http://www.sec.gov/CIK'>1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:BusinessSegmentAxis">gaap:AllOperationsMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |
      |
      |""".stripMargin

  private val xmlStringPart3 =
    """    <!-- Share Ownership Plans -->
      |    <context id="D-2007-SOP1">
      |        <!-- Share Ownership Plans, Plan 1, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ShareOwnershipPlanIdentifierAxis">gaap:ShareOwnershipPlan1Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-SOP1">
      |        <!-- Share Ownership Plans, Plan 1, Current Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ShareOwnershipPlanIdentifierAxis">gaap:ShareOwnershipPlan1Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-SOP1">
      |        <!-- Share Ownership Plans, Plan 1, Prior Period, As of -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ShareOwnershipPlanIdentifierAxis">gaap:ShareOwnershipPlan1Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |
      |
      |    <!-- Subsequent Events -->
      |    <context id="D-2007-SE-1">
      |        <!-- Subsequent Events, Loss of Uncollectable Receivable, Current Period,
      |			For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:SubsequentEventCategoryAxis">gaap:SubsequentEventRelatingToLossOfUncollectableReceivableMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-SE-2">
      |        <!-- Subsequent Events, Purchase of Business, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:SubsequentEventCategoryAxis">gaap:SubsequentEventRelatingToPurchaseOfBusinessMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |
      |
      |    <!-- Related Parties -->
      |    <context id="D-2007-RP1">
      |        <!-- Related Party 1, Related Party Transactions, All Transactions, Current
      |			Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyNameAxis">gaap:RelatedParty1Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-RP2">
      |        <!-- Related Party 2, Related Party Transactions, All Transactions, Current
      |			Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyNameAxis">gaap:RelatedParty2Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |
      |    <!-- Related Party Transactions -->
      |    <context id="D-2007-RP1T1">
      |        <!-- Related Party 1, Related Party Transactions, Transaction 1, Current
      |			Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyNameAxis">gaap:RelatedParty1Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyTransactionTypeAxis">gaap:PurchaseOrSaleOfGoodsWithRelatedPartyMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-RP1T2">
      |        <!-- Related Party 1, Related Party Transactions, Transaction 2, Current
      |			Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyNameAxis">gaap:RelatedParty1Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyTransactionTypeAxis">gaap:PurchaseOrSaleOfPropertyOrOtherAssetsWithRelatedPartyMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-RP2T1">
      |        <!-- Related Party 2, Related Party Transactions, Transaction 1, Current
      |			Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyNameAxis">gaap:RelatedParty2Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyTransactionTypeAxis">gaap:PurchaseOrSaleOfGoodsWithRelatedPartyMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-RP2T2">
      |        <!-- Related Party 2, Related Party Transactions, Transaction 2, Current
      |			Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyNameAxis">gaap:RelatedParty2Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:RelatedPartyTransactionTypeAxis">gaap:TransfersOfResearchAndDevelopmentWithRelatedPartyMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |
      |    <!-- Director Compensation -->
      |    <context id="D-2007-DIR1">
      |        <!-- Director Compensation, Director 1, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DirectorNameAxis">gaap:Director1Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-DIR2">
      |        <!-- Director Compensation, Director 2, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DirectorNameAxis">gaap:Director2Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-DIR-ALL">
      |        <!-- Director Compensation, All Directors, Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:DirectorNameAxis">gaap:AllDirectorsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |
      |
      |    <!-- Reconciliation of Cash -->
      |    <context id="D-2007-CREC-A">
      |        <!-- Reconciliation of Cash, Reconciling Item Type A, Current Period, For
      |			Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReconcilingItemTypeAxis">gaap:ReconcilingItemTypeAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-CREC-A">
      |        <!-- Reconciliation of Cash, Reconciling Item Type A, Current Period, As
      |			At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReconcilingItemTypeAxis">gaap:ReconcilingItemTypeAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-CREC-A">
      |        <!-- Reconciliation of Cash, Reconciling Item Type A, Prior Period, As
      |			At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReconcilingItemTypeAxis">gaap:ReconcilingItemTypeAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |
      |    <context id="D-2007-CREC-B">
      |        <!-- Reconciliation of Cash, Reconciling Item Type B, Current Period, For
      |			Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReconcilingItemTypeAxis">gaap:ReconcilingItemTypeBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="I-2007-CREC-B">
      |        <!-- Reconciliation of Cash, Reconciling Item Type B, Current Period, As
      |			At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReconcilingItemTypeAxis">gaap:ReconcilingItemTypeBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-CREC-B">
      |        <!-- Reconciliation of Cash, Reconciling Item Type B, Prior Period, As
      |			At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReconcilingItemTypeAxis">gaap:ReconcilingItemTypeBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2007-CREC-ALL">
      |        <!-- Reconciliation of Cash, All Reconciling Items DUPLICATE RELPACE WITH
      |			DEFAULT, Current Period, As At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReconcilingItemTypeAxis">gaap:AllReconcilingItemsOfCashAndCashEquivalentsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-CREC-ALL">
      |        <!-- Reconciliation of Cash, All Reconciling Items DUPLICATE RELPACE WITH
      |			DEFAULT, Prior Period, As At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReconcilingItemTypeAxis">gaap:AllReconcilingItemsOfCashAndCashEquivalentsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |
      |
      |
      |    <!-- Portfolio of Investments -->
      |    <context id="I-2007-INV1">
      |        <!-- Investments, T-Bill A, Current Period, As At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentDescriptionAxis">gaap:TreasuryBills3.4712010Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTermAxis">gaap:ShortTermMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentCountryAxis">gaap:USAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTypeAxis">gaap:InvestmentInGovernmentalEntitiesMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentEntityAxis">gaap:USFederalGovernmentMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentMoodysRatingAxis">gaap:APlusPlusPlusMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentSPRatingAxis">gaap:AAAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2007-INV2">
      |        <!-- Investments, T-Bill B, Current Period, As At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentDescriptionAxis">gaap:TreasuryBills3.3712011Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTermAxis">gaap:ShortTermMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentCountryAxis">gaap:USAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTypeAxis">gaap:InvestmentInGovernmentalEntitiesMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentEntityAxis">gaap:USFederalGovernmentMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentMoodysRatingAxis">gaap:APlusPlusPlusMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentSPRatingAxis">gaap:AAAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-INV3">
      |        <!-- Investments, T-Bill C, Prior Period, As At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentDescriptionAxis">gaap:TreasuryBills3.4712010Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTermAxis">gaap:ShortTermMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentCountryAxis">gaap:USAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTypeAxis">gaap:InvestmentInGovernmentalEntitiesMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentEntityAxis">gaap:USFederalGovernmentMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentMoodysRatingAxis">gaap:APlusPlusPlusMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentSPRatingAxis">gaap:AAAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2006-INV4">
      |        <!-- Investments, T-Bill D, Prior Period, As At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentDescriptionAxis">gaap:TreasuryBills3.3712011Member
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTermAxis">gaap:ShortTermMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentCountryAxis">gaap:USAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTypeAxis">gaap:InvestmentInGovernmentalEntitiesMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentEntityAxis">gaap:USFederalGovernmentMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentMoodysRatingAxis">gaap:APlusPlusPlusMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentSPRatingAxis">gaap:AAAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |    <context id="I-2007-INV-ALL">
      |        <!-- Investments, All Investments DUPLICATE USE DEFAULT, Current Period,
      |			As At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentDescriptionAxis">gaap:AllInvestmentsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTermAxis">gaap:AllInvestmentTermsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentCountryAxis">gaap:AllInvestmentCountriesDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTypeAxis">gaap:AllInvestmentTypesDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentEntityAxis">gaap:AllInvestmentEntitiesDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentMoodysRatingAxis">gaap:AllMoodysRatingsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentSPRatingAxis">gaap:AllSPRatingsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2007-12-31</instant>
      |        </period>
      |    </context>
      |""".stripMargin

  private val xmlStringPart4 =
    """    <context id="I-2006-INV-ALL">
      |        <!-- Investments, All Investments DUPLICATE USE DEFAULT, Prior Period,
      |			As At -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentDescriptionAxis">gaap:AllInvestmentsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTermAxis">gaap:AllInvestmentTermsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentCountryAxis">gaap:AllInvestmentCountriesDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentTypeAxis">gaap:AllInvestmentTypesDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentEntityAxis">gaap:AllInvestmentEntitiesDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentMoodysRatingAxis">gaap:AllMoodysRatingsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:InvestmentSPRatingAxis">gaap:AllSPRatingsDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <instant>2006-12-31</instant>
      |        </period>
      |    </context>
      |
      |
      |    <!-- Sales Analysis -->
      |    <context id="D-2007-ABC1">
      |        <!-- Sales Analysis, Customer ABC1 -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:CustomerAxis">gaap:CustomerAMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-ABC2">
      |        <!-- Sales Analysis, Customer ABC2 -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:CustomerAxis">gaap:CustomerBMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-ABC3">
      |        <!-- Sales Analysis, Customer ABC3 -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:CustomerAxis">gaap:CustomerCMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id="D-2007-ABC-ALL">
      |        <!-- Sales Analysis, Customers, All, DUPLICATE USE DEFAULT -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:CustomerAxis">gaap:AllCustomersDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |
      |
      |    <!-- PPE Classes -->
      |    <context id='D-2007-PPE-Land'>
      |        <!-- Land [Member], Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember
      |                        dimension="gaap:PropertyPlantAndEquipmentDescriptionOfMajorClassAxis">gaap:LandMember</xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2007-PPE-Buildings'>
      |        <!-- Buildings [Member], Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember
      |                        dimension="gaap:PropertyPlantAndEquipmentDescriptionOfMajorClassAxis">gaap:BuildingsMember</xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2007-PPE-FurnitureFixtures'>
      |        <!-- FurnitureAndFixtures [Member], Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember
      |                        dimension="gaap:PropertyPlantAndEquipmentDescriptionOfMajorClassAxis">gaap:FurnitureAndFixturesMember</xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |    <context id='D-2007-PPE-Other'>
      |        <!-- Other [Member], Current Period, For Period -->
      |        <entity>
      |            <identifier scheme="http://www.sec.gov/CIK">1234567890</identifier>
      |            <segment>
      |                <xbrldi:explicitMember dimension="gaap:EntityAxis">gaap:ABCCompanyDomain
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember
      |                        dimension="gaap:PropertyPlantAndEquipmentDescriptionOfMajorClassAxis">gaap:OtherPropertyPlantAndEquipmentMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:VerificationAxis">gaap:UnqualifiedOpinionMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:PremiseAxis">gaap:ActualMember
      |                </xbrldi:explicitMember>
      |                <xbrldi:explicitMember dimension="gaap:ReportDateAxis">gaap:ReportedAsOfMarch182008Member
      |                </xbrldi:explicitMember>
      |            </segment>
      |        </entity>
      |        <period>
      |            <startDate>2007-01-01</startDate>
      |            <endDate>2007-12-31</endDate>
      |        </period>
      |    </context>
      |
      |
      |    <!-- Units -->
      |    <unit id='U-Monetary'>
      |        <!-- US Dollars -->
      |        <measure>iso4217:USD</measure>
      |    </unit>
      |    <unit id="U-Shares">
      |        <!-- Shares -->
      |        <measure>shares</measure>
      |    </unit>
      |    <unit id="U-Pure">
      |        <!-- Pure; no measure, pure number -->
      |        <measure>pure</measure>
      |    </unit>
      |
      |
      |
      |
      |
      |
      |    <!-- Financial Highlights -->
      |    <gaap:RevenuesNet contextRef="D-2005" unitRef="U-Monetary"
      |                      decimals="INF">0</gaap:RevenuesNet>
      |    <gaap:RevenuesNet contextRef="D-2004" unitRef="U-Monetary"
      |                      decimals="INF">0</gaap:RevenuesNet>
      |    <gaap:RevenuesNet contextRef="D-2003" unitRef="U-Monetary"
      |                      decimals="INF">0</gaap:RevenuesNet>
      |
      |    <gaap:IncomeLossFromContinuingOperations
      |            contextRef="D-2005" unitRef="U-Monetary" decimals="INF">-4000
      |    </gaap:IncomeLossFromContinuingOperations>
      |    <gaap:IncomeLossFromContinuingOperations
      |            contextRef="D-2004" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncomeLossFromContinuingOperations>
      |    <gaap:IncomeLossFromContinuingOperations
      |            contextRef="D-2003" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncomeLossFromContinuingOperations>
      |
      |    <gaap:NetIncomeLoss contextRef="D-2005" unitRef="U-Monetary"
      |                        decimals="INF">-4000</gaap:NetIncomeLoss>
      |    <gaap:NetIncomeLoss contextRef="D-2004" unitRef="U-Monetary"
      |                        decimals="INF">0</gaap:NetIncomeLoss>
      |    <gaap:NetIncomeLoss contextRef="D-2003" unitRef="U-Monetary"
      |                        decimals="INF">0</gaap:NetIncomeLoss>
      |
      |    <gaap:CashFlowProvidedByUsedInOperatingActivitiesNet
      |            contextRef="D-2005" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashFlowProvidedByUsedInOperatingActivitiesNet>
      |    <gaap:CashFlowProvidedByUsedInOperatingActivitiesNet
      |            contextRef="D-2004" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashFlowProvidedByUsedInOperatingActivitiesNet>
      |    <gaap:CashFlowProvidedByUsedInOperatingActivitiesNet
      |            contextRef="D-2003" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashFlowProvidedByUsedInOperatingActivitiesNet>
      |
      |    <gaap:CapitalAdditions contextRef="D-2006"
      |                           unitRef="U-Monetary" decimals="INF">650</gaap:CapitalAdditions>
      |    <gaap:CapitalAdditions contextRef="D-2005"
      |                           unitRef="U-Monetary" decimals="INF">550</gaap:CapitalAdditions>
      |    <gaap:CapitalAdditions contextRef="D-2004"
      |                           unitRef="U-Monetary" decimals="INF">450</gaap:CapitalAdditions>
      |    <gaap:CapitalAdditions contextRef="D-2003"
      |                           unitRef="U-Monetary" decimals="INF">350</gaap:CapitalAdditions>
      |
      |    <gaap:AverageNumberEmployees contextRef="D-2005"
      |                                 unitRef="U-Pure" decimals="INF">250</gaap:AverageNumberEmployees>
      |    <gaap:AverageNumberEmployees contextRef="D-2004"
      |                                 unitRef="U-Pure" decimals="INF">240</gaap:AverageNumberEmployees>
      |    <gaap:AverageNumberEmployees contextRef="D-2003"
      |                                 unitRef="U-Pure" decimals="INF">220</gaap:AverageNumberEmployees>
      |
      |
      |    <!-- Balance Sheet -->
      |    <gaap:CashAndCashEquivalents id="Item-01"
      |                                 contextRef="I-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:CashAndCashEquivalents>
      |    <gaap:ReceivablesNetCurrent id="Item-02"
      |                                contextRef="I-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:ReceivablesNetCurrent>
      |
      |    <gaap:Inventory contextRef="I-2007" unitRef="U-Monetary"
      |                    decimals="INF">1000</gaap:Inventory>
      |    <gaap:PrepaidExpenses contextRef="I-2007" unitRef="U-Monetary"
      |                          decimals="INF">1000</gaap:PrepaidExpenses>
      |    <gaap:OtherAssetsCurrent contextRef="I-2007"
      |                             unitRef="U-Monetary" decimals="INF">1000</gaap:OtherAssetsCurrent>
      |    <gaap:AssetsCurrent contextRef="I-2007" unitRef="U-Monetary"
      |                        decimals="INF">5000</gaap:AssetsCurrent>
      |    <gaap:Land contextRef="I-2007" unitRef="U-Monetary" decimals="INF">
      |        1000</gaap:Land>
      |    <gaap:BuildingsNet contextRef="I-2007" unitRef="U-Monetary"
      |                       decimals="INF">1000</gaap:BuildingsNet>
      |    <gaap:FurnitureAndFixturesNet contextRef="I-2007"
      |                                  unitRef="U-Monetary" decimals="INF">1000</gaap:FurnitureAndFixturesNet>
      |    <gaap:OtherPropertyPlantAndEquipmentNet
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherPropertyPlantAndEquipmentNet>
      |    <gaap:PropertyPlantAndEquipmentNet
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:PropertyPlantAndEquipmentNet>
      |    <gaap:InvestmentInAffiliates contextRef="I-2007"
      |                                 unitRef="U-Monetary" decimals="INF">0</gaap:InvestmentInAffiliates>
      |    <gaap:OtherAssetsNoncurrent contextRef="I-2007"
      |                                unitRef="U-Monetary" decimals="INF">3000</gaap:OtherAssetsNoncurrent>
      |    <gaap:AssetsNoncurrent contextRef="I-2007"
      |                           unitRef="U-Monetary" decimals="INF">7000</gaap:AssetsNoncurrent>
      |    <gaap:Assets contextRef="I-2007" unitRef="U-Monetary"
      |                 decimals="INF">12000</gaap:Assets>
      |    <gaap:PayablesAndAccruals contextRef="I-2007"
      |                              unitRef="U-Monetary" decimals="INF">3000</gaap:PayablesAndAccruals>
      |    <gaap:AccountsPayable contextRef="I-2007" unitRef="U-Monetary"
      |                          decimals="INF">1000</gaap:AccountsPayable>
      |    <gaap:AccruedInterestPayable contextRef="I-2007"
      |                                 unitRef="U-Monetary" decimals="INF">1000</gaap:AccruedInterestPayable>
      |    <gaap:AccruedExpenses contextRef="I-2007" unitRef="U-Monetary"
      |                          decimals="INF">1000</gaap:AccruedExpenses>
      |    <gaap:OtherPayablesAndAccruals
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherPayablesAndAccruals>
      |    <gaap:LongTermDebtCurrent contextRef="I-2007"
      |                              unitRef="U-Monetary" decimals="INF">1000</gaap:LongTermDebtCurrent>
      |    <gaap:OtherLiabilitiesCurrent contextRef="I-2007"
      |                                  unitRef="U-Monetary" decimals="INF">1000</gaap:OtherLiabilitiesCurrent>
      |    <gaap:LiabilitiesCurrent contextRef="I-2007"
      |                             unitRef="U-Monetary" decimals="INF">5000</gaap:LiabilitiesCurrent>
      |    <gaap:LongTermDebtNoncurrent contextRef="I-2007"
      |                                 unitRef="U-Monetary" decimals="INF">500</gaap:LongTermDebtNoncurrent>
      |    <gaap:OtherLiabilitiesNoncurrent
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:OtherLiabilitiesNoncurrent>
      |    <gaap:LiabilitiesNoncurrent contextRef="I-2007"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:LiabilitiesNoncurrent>
      |    <gaap:Liabilities contextRef="I-2007" unitRef="U-Monetary"
      |                      decimals="INF">6000</gaap:Liabilities>
      |    <gaap:CommonStock contextRef="I-2007" unitRef="U-Monetary"
      |                      decimals="INF">1000</gaap:CommonStock>
      |    <gaap:PreferredStock contextRef="I-2007" unitRef="U-Monetary"
      |                         decimals="INF">2000</gaap:PreferredStock>
      |    <gaap:AdditionalPaidInCapital contextRef="I-2007"
      |                                  unitRef="U-Monetary" decimals="INF">2000</gaap:AdditionalPaidInCapital>
      |    <gaap:RetainedEarningsAccumulatedLosses
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:RetainedEarningsAccumulatedLosses>
      |    <gaap:Equity contextRef="I-2007" unitRef="U-Monetary"
      |                 decimals="INF">6000</gaap:Equity>
      |    <gaap:LiabilitiesAndEquity contextRef="I-2007"
      |                               unitRef="U-Monetary" decimals="INF">12000</gaap:LiabilitiesAndEquity>
      |
      |    <gaap:CashAndCashEquivalents contextRef="I-2006"
      |                                 unitRef="U-Monetary" decimals="INF">1000</gaap:CashAndCashEquivalents>
      |    <gaap:ReceivablesNetCurrent contextRef="I-2006"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:ReceivablesNetCurrent>
      |    <gaap:Inventory contextRef="I-2006" unitRef="U-Monetary"
      |                    decimals="INF">1000</gaap:Inventory>
      |    <gaap:PrepaidExpenses contextRef="I-2006" unitRef="U-Monetary"
      |                          decimals="INF">1000</gaap:PrepaidExpenses>
      |    <gaap:OtherAssetsCurrent contextRef="I-2006"
      |                             unitRef="U-Monetary" decimals="INF">1000</gaap:OtherAssetsCurrent>
      |    <gaap:AssetsCurrent contextRef="I-2006" unitRef="U-Monetary"
      |                        decimals="INF">5000</gaap:AssetsCurrent>
      |    <gaap:Land contextRef="I-2006" unitRef="U-Monetary" decimals="INF">
      |        1000</gaap:Land>
      |    <gaap:BuildingsNet contextRef="I-2006" unitRef="U-Monetary"
      |                       decimals="INF">1000</gaap:BuildingsNet>
      |    <gaap:FurnitureAndFixturesNet contextRef="I-2006"
      |                                  unitRef="U-Monetary" decimals="INF">1000</gaap:FurnitureAndFixturesNet>
      |    <gaap:OtherPropertyPlantAndEquipmentNet
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherPropertyPlantAndEquipmentNet>
      |    <gaap:PropertyPlantAndEquipmentNet
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:PropertyPlantAndEquipmentNet>
      |    <gaap:InvestmentInAffiliates contextRef="I-2006"
      |                                 unitRef="U-Monetary" decimals="INF">0</gaap:InvestmentInAffiliates>
      |    <gaap:OtherAssetsNoncurrent contextRef="I-2006"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:OtherAssetsNoncurrent>
      |    <gaap:AssetsNoncurrent contextRef="I-2006"
      |                           unitRef="U-Monetary" decimals="INF">5000</gaap:AssetsNoncurrent>
      |    <gaap:Assets contextRef="I-2006" unitRef="U-Monetary"
      |                 decimals="INF">10000</gaap:Assets>
      |
      |    <!-- Note the IDs which indicate reclassification. -->
      |
      |    <gaap:PayablesAndAccruals contextRef="I-2006"
      |                              unitRef="U-Monetary" decimals="INF">3000</gaap:PayablesAndAccruals>
      |    <gaap:AccountsPayable id="Reclassification-01"
      |                          contextRef="I-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AccountsPayable>
      |    <gaap:AccruedInterestPayable id="Reclassification-02"
      |                                 contextRef="I-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AccruedInterestPayable>
      |    <gaap:AccruedExpenses id="Reclassification-03"
      |                          contextRef="I-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AccruedExpenses>
      |    <gaap:OtherPayablesAndAccruals id="Reclassification-04"
      |                                   contextRef="I-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherPayablesAndAccruals>
      |
      |    <gaap:LongTermDebtCurrent contextRef="I-2006"
      |                              unitRef="U-Monetary" decimals="INF">1000</gaap:LongTermDebtCurrent>
      |    <gaap:OtherLiabilitiesCurrent contextRef="I-2006"
      |                                  unitRef="U-Monetary" decimals="INF">1000</gaap:OtherLiabilitiesCurrent>
      |    <gaap:LiabilitiesCurrent contextRef="I-2006"
      |                             unitRef="U-Monetary" decimals="INF">5000</gaap:LiabilitiesCurrent>
      |    <gaap:LongTermDebtNoncurrent contextRef="I-2006"
      |                                 unitRef="U-Monetary" decimals="INF">500</gaap:LongTermDebtNoncurrent>
      |    <gaap:OtherLiabilitiesNoncurrent
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:OtherLiabilitiesNoncurrent>
      |    <gaap:LiabilitiesNoncurrent contextRef="I-2006"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:LiabilitiesNoncurrent>
      |    <gaap:Liabilities contextRef="I-2006" unitRef="U-Monetary"
      |                      decimals="INF">6000</gaap:Liabilities>
      |    <gaap:CommonStock contextRef="I-2006" unitRef="U-Monetary"
      |                      decimals="INF">1000</gaap:CommonStock>
      |    <gaap:PreferredStock contextRef="I-2006" unitRef="U-Monetary"
      |                         decimals="INF">1000</gaap:PreferredStock>
      |    <gaap:AdditionalPaidInCapital contextRef="I-2006"
      |                                  unitRef="U-Monetary" decimals="INF">1000</gaap:AdditionalPaidInCapital>
      |    <gaap:RetainedEarningsAccumulatedLosses
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:RetainedEarningsAccumulatedLosses>
      |    <gaap:Equity contextRef="I-2006" unitRef="U-Monetary"
      |                 decimals="INF">4000</gaap:Equity>
      |    <gaap:LiabilitiesAndEquity contextRef="I-2006"
      |                               unitRef="U-Monetary" decimals="INF">10000</gaap:LiabilitiesAndEquity>
      |
      |    <gaap:LongTermDebt contextRef="I-2007" unitRef="U-Monetary"
      |                       decimals="INF">1500</gaap:LongTermDebt>
      |    <gaap:LongTermDebt contextRef="I-2006" unitRef="U-Monetary"
      |                       decimals="INF">1500</gaap:LongTermDebt>
      |
      |    <gaap:OtherAssets contextRef="I-2007" unitRef="U-Monetary"
      |                      decimals="INF">4000</gaap:OtherAssets>
      |    <gaap:OtherAssets contextRef="I-2006" unitRef="U-Monetary"
      |                      decimals="INF">2000</gaap:OtherAssets>
      |
      |    <gaap:OtherLiabilities contextRef="I-2007"
      |                           unitRef="U-Monetary" decimals="INF">1500</gaap:OtherLiabilities>
      |    <gaap:OtherLiabilities contextRef="I-2006"
      |                           unitRef="U-Monetary" decimals="INF">1500</gaap:OtherLiabilities>
      |
      |    <gaap:Land contextRef="I-2005" unitRef="U-Monetary" decimals="INF">
      |        1000</gaap:Land>
      |    <gaap:BuildingsNet contextRef="I-2005" unitRef="U-Monetary"
      |                       decimals="INF">1000</gaap:BuildingsNet>
      |    <gaap:FurnitureAndFixturesNet contextRef="I-2005"
      |                                  unitRef="U-Monetary" decimals="INF">1000</gaap:FurnitureAndFixturesNet>
      |    <gaap:OtherPropertyPlantAndEquipmentNet
      |            contextRef="I-2005" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherPropertyPlantAndEquipmentNet>
      |    <gaap:PropertyPlantAndEquipmentNet
      |            contextRef="I-2005" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:PropertyPlantAndEquipmentNet>
      |
      |    <gaap:Equity contextRef="I-2005" unitRef="U-Monetary"
      |                 decimals="INF">1000</gaap:Equity>
      |    <gaap:CommonStock contextRef="I-2005" unitRef="U-Monetary"
      |                      decimals="INF">1000</gaap:CommonStock>
      |    <gaap:PreferredStock contextRef="I-2005" unitRef="U-Monetary"
      |                         decimals="INF">0</gaap:PreferredStock>
      |    <gaap:AdditionalPaidInCapital contextRef="I-2005"
      |                                  unitRef="U-Monetary" decimals="INF">0</gaap:AdditionalPaidInCapital>
      |    <gaap:RetainedEarningsAccumulatedLosses
      |            contextRef="I-2005" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:RetainedEarningsAccumulatedLosses>
      |
      |
      |
      |    <!-- Balance Sheet, Depository and Lending Institutions -->
      |
      |    <gaap:Loans contextRef="I-2007" unitRef="U-Monetary"
      |                decimals="INF">1000</gaap:Loans>
      |    <gaap:Loans contextRef="I-2006" unitRef="U-Monetary"
      |                decimals="INF">1000</gaap:Loans>
      |
      |    <gaap:Investments contextRef="I-2007" unitRef="U-Monetary"
      |                      decimals="INF">2000</gaap:Investments>
      |    <gaap:Investments contextRef="I-2006" unitRef="U-Monetary"
      |                      decimals="INF">2000</gaap:Investments>
      |
      |    <gaap:Deposits contextRef="I-2007" unitRef="U-Monetary"
      |                   decimals="INF">1500</gaap:Deposits>
      |    <gaap:Deposits contextRef="I-2006" unitRef="U-Monetary"
      |                   decimals="INF">1500</gaap:Deposits>
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |    <!-- Balance Sheet, Classes of Stock -->
      |
      |    <!-- Preferred -->
      |    <gaap:PreferredStockAmount contextRef="I-2007-PS-All"
      |                               unitRef="U-Monetary" decimals="INF">2000</gaap:PreferredStockAmount>
      |    <gaap:PreferredStockShareSubscriptions
      |            contextRef="I-2007-PS-All" unitRef="U-Shares" decimals="INF">20000
      |    </gaap:PreferredStockShareSubscriptions>
      |    <gaap:PreferredStockSharesAuthorized
      |            contextRef="I-2007-PS-All" unitRef="U-Shares" decimals="INF">20000
      |    </gaap:PreferredStockSharesAuthorized>
      |    <gaap:PreferredStockSharesIssued
      |            contextRef="I-2007-PS-All" unitRef="U-Shares" decimals="INF">20000
      |    </gaap:PreferredStockSharesIssued>
      |    <gaap:PreferredStockSharesOutstanding
      |            contextRef="I-2007-PS-All" unitRef="U-Shares" decimals="INF">6000
      |    </gaap:PreferredStockSharesOutstanding>
      |
      |    <gaap:PreferredStockAmount contextRef="I-2007-PSA"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:PreferredStockAmount>
      |    <gaap:PreferredStockParValuePerShare
      |            contextRef="I-2007-PSA" unitRef="U-Pure" decimals="INF">1
      |    </gaap:PreferredStockParValuePerShare>
      |    <gaap:PreferredStockShareSubscriptions
      |            contextRef="I-2007-PSA" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockShareSubscriptions>
      |    <gaap:PreferredStockSharesAuthorized
      |            contextRef="I-2007-PSA" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockSharesAuthorized>
      |    <gaap:PreferredStockSharesIssued
      |            contextRef="I-2007-PSA" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockSharesIssued>
      |    <gaap:PreferredStockSharesOutstanding
      |            contextRef="I-2007-PSA" unitRef="U-Shares" decimals="INF">3000
      |    </gaap:PreferredStockSharesOutstanding>
      |
      |    <gaap:PreferredStockAmount contextRef="I-2007-PSB"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:PreferredStockAmount>
      |    <gaap:PreferredStockParValuePerShare
      |            contextRef="I-2007-PSB" unitRef="U-Pure" decimals="INF">1
      |    </gaap:PreferredStockParValuePerShare>
      |    <gaap:PreferredStockShareSubscriptions
      |            contextRef="I-2007-PSB" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockShareSubscriptions>
      |    <gaap:PreferredStockSharesAuthorized
      |            contextRef="I-2007-PSB" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockSharesAuthorized>
      |    <gaap:PreferredStockSharesIssued
      |            contextRef="I-2007-PSB" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockSharesIssued>
      |    <gaap:PreferredStockSharesOutstanding
      |            contextRef="I-2007-PSB" unitRef="U-Shares" decimals="INF">3000
      |    </gaap:PreferredStockSharesOutstanding>
      |
      |
      |    <gaap:PreferredStockAmount contextRef="I-2006-PS-All"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:PreferredStockAmount>
      |    <gaap:PreferredStockShareSubscriptions
      |            contextRef="I-2006-PS-All" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockShareSubscriptions>
      |    <gaap:PreferredStockSharesAuthorized
      |            contextRef="I-2006-PS-All" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockSharesAuthorized>
      |    <gaap:PreferredStockSharesIssued
      |            contextRef="I-2006-PS-All" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:PreferredStockSharesIssued>
      |    <gaap:PreferredStockSharesOutstanding
      |            contextRef="I-2006-PS-All" unitRef="U-Shares" decimals="INF">3000
      |    </gaap:PreferredStockSharesOutstanding>
      |
      |    <gaap:PreferredStockAmount contextRef="I-2006-PSA"
      |                               unitRef="U-Monetary" decimals="INF">500</gaap:PreferredStockAmount>
      |    <gaap:PreferredStockParValuePerShare
      |            contextRef="I-2006-PSA" unitRef="U-Pure" decimals="INF">1
      |    </gaap:PreferredStockParValuePerShare>
      |    <gaap:PreferredStockShareSubscriptions
      |            contextRef="I-2006-PSA" unitRef="U-Shares" decimals="INF">5000
      |    </gaap:PreferredStockShareSubscriptions>
      |    <gaap:PreferredStockSharesAuthorized
      |            contextRef="I-2006-PSA" unitRef="U-Shares" decimals="INF">5000
      |    </gaap:PreferredStockSharesAuthorized>
      |    <gaap:PreferredStockSharesIssued
      |            contextRef="I-2006-PSA" unitRef="U-Shares" decimals="INF">5000
      |    </gaap:PreferredStockSharesIssued>
      |    <gaap:PreferredStockSharesOutstanding
      |            contextRef="I-2006-PSA" unitRef="U-Shares" decimals="INF">1500
      |    </gaap:PreferredStockSharesOutstanding>
      |
      |    <gaap:PreferredStockAmount contextRef="I-2006-PSB"
      |                               unitRef="U-Monetary" decimals="INF">500</gaap:PreferredStockAmount>
      |    <gaap:PreferredStockParValuePerShare
      |            contextRef="I-2006-PSB" unitRef="U-Pure" decimals="INF">1
      |    </gaap:PreferredStockParValuePerShare>
      |    <gaap:PreferredStockShareSubscriptions
      |            contextRef="I-2006-PSB" unitRef="U-Shares" decimals="INF">5000
      |    </gaap:PreferredStockShareSubscriptions>
      |    <gaap:PreferredStockSharesAuthorized
      |            contextRef="I-2006-PSB" unitRef="U-Shares" decimals="INF">5000
      |    </gaap:PreferredStockSharesAuthorized>
      |    <gaap:PreferredStockSharesIssued
      |            contextRef="I-2006-PSB" unitRef="U-Shares" decimals="INF">5000
      |    </gaap:PreferredStockSharesIssued>
      |    <gaap:PreferredStockSharesOutstanding
      |            contextRef="I-2006-PSB" unitRef="U-Shares" decimals="INF">1500
      |    </gaap:PreferredStockSharesOutstanding>
      |
      |    <!-- Common -->
      |    <gaap:CommonStockAmount contextRef="I-2007-CS-All"
      |                            unitRef="U-Monetary" decimals="INF">1000</gaap:CommonStockAmount>
      |    <gaap:CommonStockShareSubscriptions
      |            contextRef="I-2007-CS-All" unitRef="U-Shares" decimals="INF">20000
      |    </gaap:CommonStockShareSubscriptions>
      |    <gaap:CommonStockSharesAuthorized
      |            contextRef="I-2007-CS-All" unitRef="U-Shares" decimals="INF">20000
      |    </gaap:CommonStockSharesAuthorized>
      |    <gaap:CommonStockSharesIssued contextRef="I-2007-CS-All"
      |                                  unitRef="U-Shares" decimals="INF">20000</gaap:CommonStockSharesIssued>
      |    <gaap:CommonStockSharesOutstanding
      |            contextRef="I-2007-CS-All" unitRef="U-Shares" decimals="INF">6000
      |    </gaap:CommonStockSharesOutstanding>
      |
      |    <gaap:CommonStockAmount contextRef="I-2007-CSA"
      |                            unitRef="U-Monetary" decimals="INF">500</gaap:CommonStockAmount>
      |    <gaap:CommonStockParValuePerShare
      |            contextRef="I-2007-CSA" unitRef="U-Pure" decimals="INF">1
      |    </gaap:CommonStockParValuePerShare>
      |    <gaap:CommonStockShareSubscriptions
      |            contextRef="I-2007-CSA" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:CommonStockShareSubscriptions>
      |    <gaap:CommonStockSharesAuthorized
      |            contextRef="I-2007-CSA" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:CommonStockSharesAuthorized>
      |    <gaap:CommonStockSharesIssued contextRef="I-2007-CSA"
      |                                  unitRef="U-Shares" decimals="INF">10000</gaap:CommonStockSharesIssued>
      |    <gaap:CommonStockSharesOutstanding
      |            contextRef="I-2007-CSA" unitRef="U-Shares" decimals="INF">3000
      |    </gaap:CommonStockSharesOutstanding>
      |
      |    <gaap:CommonStockAmount contextRef="I-2007-CSB"
      |                            unitRef="U-Monetary" decimals="INF">500</gaap:CommonStockAmount>
      |    <gaap:CommonStockParValuePerShare
      |            contextRef="I-2007-CSB" unitRef="U-Pure" decimals="INF">1
      |    </gaap:CommonStockParValuePerShare>
      |    <gaap:CommonStockShareSubscriptions
      |            contextRef="I-2007-CSB" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:CommonStockShareSubscriptions>
      |    <gaap:CommonStockSharesAuthorized
      |            contextRef="I-2007-CSB" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:CommonStockSharesAuthorized>
      |    <gaap:CommonStockSharesIssued contextRef="I-2007-CSB"
      |                                  unitRef="U-Shares" decimals="INF">10000</gaap:CommonStockSharesIssued>
      |    <gaap:CommonStockSharesOutstanding
      |            contextRef="I-2007-CSB" unitRef="U-Shares" decimals="INF">3000
      |    </gaap:CommonStockSharesOutstanding>
      |
      |
      |""".stripMargin

  private val xmlStringPart5 =
    """    <gaap:CommonStockAmount contextRef="I-2006-CS-All"
      |                            unitRef="U-Monetary" decimals="INF">1000</gaap:CommonStockAmount>
      |    <gaap:CommonStockShareSubscriptions
      |            contextRef="I-2006-CS-All" unitRef="U-Shares" decimals="INF">20000
      |    </gaap:CommonStockShareSubscriptions>
      |    <gaap:CommonStockSharesAuthorized
      |            contextRef="I-2006-CS-All" unitRef="U-Shares" decimals="INF">20000
      |    </gaap:CommonStockSharesAuthorized>
      |    <gaap:CommonStockSharesIssued contextRef="I-2006-CS-All"
      |                                  unitRef="U-Shares" decimals="INF">20000</gaap:CommonStockSharesIssued>
      |    <gaap:CommonStockSharesOutstanding
      |            contextRef="I-2006-CS-All" unitRef="U-Shares" decimals="INF">6000
      |    </gaap:CommonStockSharesOutstanding>
      |
      |    <gaap:CommonStockAmount contextRef="I-2006-CSA"
      |                            unitRef="U-Monetary" decimals="INF">500</gaap:CommonStockAmount>
      |    <gaap:CommonStockParValuePerShare
      |            contextRef="I-2006-CSA" unitRef="U-Pure" decimals="INF">1
      |    </gaap:CommonStockParValuePerShare>
      |    <gaap:CommonStockShareSubscriptions
      |            contextRef="I-2006-CSA" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:CommonStockShareSubscriptions>
      |    <gaap:CommonStockSharesAuthorized
      |            contextRef="I-2006-CSA" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:CommonStockSharesAuthorized>
      |    <gaap:CommonStockSharesIssued contextRef="I-2006-CSA"
      |                                  unitRef="U-Shares" decimals="INF">10000</gaap:CommonStockSharesIssued>
      |    <gaap:CommonStockSharesOutstanding
      |            contextRef="I-2006-CSA" unitRef="U-Shares" decimals="INF">3000
      |    </gaap:CommonStockSharesOutstanding>
      |
      |    <gaap:CommonStockAmount contextRef="I-2006-CSB"
      |                            unitRef="U-Monetary" decimals="INF">500</gaap:CommonStockAmount>
      |    <gaap:CommonStockParValuePerShare
      |            contextRef="I-2006-CSB" unitRef="U-Pure" decimals="INF">1
      |    </gaap:CommonStockParValuePerShare>
      |    <gaap:CommonStockShareSubscriptions
      |            contextRef="I-2006-CSB" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:CommonStockShareSubscriptions>
      |    <gaap:CommonStockSharesAuthorized
      |            contextRef="I-2006-CSB" unitRef="U-Shares" decimals="INF">10000
      |    </gaap:CommonStockSharesAuthorized>
      |    <gaap:CommonStockSharesIssued contextRef="I-2006-CSB"
      |                                  unitRef="U-Shares" decimals="INF">10000</gaap:CommonStockSharesIssued>
      |    <gaap:CommonStockSharesOutstanding
      |            contextRef="I-2006-CSB" unitRef="U-Shares" decimals="INF">3000
      |    </gaap:CommonStockSharesOutstanding>
      |
      |
      |
      |
      |
      |
      |    <!-- Income Statement -->
      |    <gaap:RevenuesGross contextRef="D-2007" unitRef="U-Monetary"
      |                        decimals="INF">5000</gaap:RevenuesGross>
      |    <gaap:ReturnsAndAllowances contextRef="D-2007"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:ReturnsAndAllowances>
      |    <gaap:RevenuesNet contextRef="D-2007" unitRef="U-Monetary"
      |                      decimals="INF">4000</gaap:RevenuesNet>
      |    <gaap:CostOfSales contextRef="D-2007" unitRef="U-Monetary"
      |                      decimals="INF">1000</gaap:CostOfSales>
      |    <gaap:GrossProfitLoss contextRef="D-2007" unitRef="U-Monetary"
      |                          decimals="INF">3000</gaap:GrossProfitLoss>
      |    <gaap:OperatingExpenses contextRef="D-2007"
      |                            unitRef="U-Monetary" decimals="INF">1000</gaap:OperatingExpenses>
      |    <gaap:OperatingIncome contextRef="D-2007" unitRef="U-Monetary"
      |                          decimals="INF">1000</gaap:OperatingIncome>
      |    <gaap:OperatingIncomeLoss contextRef="D-2007"
      |                              unitRef="U-Monetary" decimals="INF">3000</gaap:OperatingIncomeLoss>
      |    <gaap:InterestExpenseIncome contextRef="D-2007"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:InterestExpenseIncome>
      |    <gaap:OtherNonoperatingExpensesIncome
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherNonoperatingExpensesIncome>
      |    <gaap:NonoperatingIncomeLoss contextRef="D-2007"
      |                                 unitRef="U-Monetary" decimals="INF">-2000</gaap:NonoperatingIncomeLoss>
      |    <gaap:IncomeLossFromContinuingOperationsBeforeIncomeTaxes
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:IncomeLossFromContinuingOperationsBeforeIncomeTaxes>
      |    <gaap:IncomeTaxExpenseBenefit contextRef="D-2007"
      |                                  unitRef="U-Monetary" decimals="INF">500</gaap:IncomeTaxExpenseBenefit>
      |    <gaap:IncomeLossFromContinuingOperations
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:IncomeLossFromContinuingOperations>
      |    <gaap:IncomeLossFromDiscontinuedOperationsNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncomeLossFromDiscontinuedOperationsNet>
      |
      |    <gaap:NetIncomeLoss contextRef="D-2007" unitRef="U-Monetary"
      |                        decimals="INF">500</gaap:NetIncomeLoss>
      |
      |    <gaap:RevenuesGross contextRef="D-2006" unitRef="U-Monetary"
      |                        decimals="INF">1000</gaap:RevenuesGross>
      |    <gaap:ReturnsAndAllowances contextRef="D-2006"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:ReturnsAndAllowances>
      |    <gaap:RevenuesNet contextRef="D-2006" unitRef="U-Monetary"
      |                      decimals="INF">0</gaap:RevenuesNet>
      |    <gaap:CostOfSales contextRef="D-2006" unitRef="U-Monetary"
      |                      decimals="INF">1000</gaap:CostOfSales>
      |    <gaap:GrossProfitLoss contextRef="D-2006" unitRef="U-Monetary"
      |                          decimals="INF">-1000</gaap:GrossProfitLoss>
      |    <gaap:OperatingExpenses contextRef="D-2006"
      |                            unitRef="U-Monetary" decimals="INF">1000</gaap:OperatingExpenses>
      |    <gaap:OperatingIncome contextRef="D-2006" unitRef="U-Monetary"
      |                          decimals="INF">1000</gaap:OperatingIncome>
      |    <gaap:OperatingIncomeLoss contextRef="D-2006"
      |                              unitRef="U-Monetary" decimals="INF">-1000</gaap:OperatingIncomeLoss>
      |    <gaap:InterestExpenseIncome contextRef="D-2006"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:InterestExpenseIncome>
      |    <gaap:OtherNonoperatingExpensesIncome
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherNonoperatingExpensesIncome>
      |    <gaap:NonoperatingIncomeLoss contextRef="D-2006"
      |                                 unitRef="U-Monetary" decimals="INF">-2000</gaap:NonoperatingIncomeLoss>
      |    <gaap:IncomeLossFromContinuingOperationsBeforeIncomeTaxes
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">-3000
      |    </gaap:IncomeLossFromContinuingOperationsBeforeIncomeTaxes>
      |    <gaap:IncomeTaxExpenseBenefit contextRef="D-2006"
      |                                  unitRef="U-Monetary" decimals="INF">1000</gaap:IncomeTaxExpenseBenefit>
      |
      |    <gaap:IncomeLossFromContinuingOperations
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">-4000
      |    </gaap:IncomeLossFromContinuingOperations>
      |    <gaap:IncomeLossFromDiscontinuedOperationsNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncomeLossFromDiscontinuedOperationsNet>
      |
      |    <gaap:NetIncomeLoss contextRef="D-2006" unitRef="U-Monetary"
      |                        decimals="INF">-4000</gaap:NetIncomeLoss>
      |
      |    <gaap:EarningsLossPerShare contextRef="D-2007"
      |                               unitRef="U-Monetary" decimals="INF">1.25</gaap:EarningsLossPerShare>
      |    <gaap:EarningsLossPerShare contextRef="D-2006"
      |                               unitRef="U-Monetary" decimals="INF">1.25</gaap:EarningsLossPerShare>
      |
      |
      |
      |    <!-- Income Statement, Financial Institutions -->
      |    <gaap:InterestIncomeFinancialInstitutions
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">5000
      |    </gaap:InterestIncomeFinancialInstitutions>
      |    <gaap:InterestExpenseFinancialInstitutions
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">3000
      |    </gaap:InterestExpenseFinancialInstitutions>
      |    <gaap:InterestIncomeExpenseNetFinancialInstitutions
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:InterestIncomeExpenseNetFinancialInstitutions>
      |    <gaap:FeesAndCommissionIncome contextRef="D-2007"
      |                                  unitRef="U-Monetary" decimals="INF">6000</gaap:FeesAndCommissionIncome>
      |    <gaap:FeesAndCommissionExpense
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">5000
      |    </gaap:FeesAndCommissionExpense>
      |    <gaap:FeesAndCommissionNet contextRef="D-2007"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:FeesAndCommissionNet>
      |
      |    <gaap:InterestIncomeFinancialInstitutions
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">5000
      |    </gaap:InterestIncomeFinancialInstitutions>
      |    <gaap:InterestExpenseFinancialInstitutions
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">7000
      |    </gaap:InterestExpenseFinancialInstitutions>
      |    <gaap:InterestIncomeExpenseNetFinancialInstitutions
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">-2000
      |    </gaap:InterestIncomeExpenseNetFinancialInstitutions>
      |    <gaap:FeesAndCommissionIncome contextRef="D-2006"
      |                                  unitRef="U-Monetary" decimals="INF">6000</gaap:FeesAndCommissionIncome>
      |    <gaap:FeesAndCommissionExpense
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">5000
      |    </gaap:FeesAndCommissionExpense>
      |    <gaap:FeesAndCommissionNet contextRef="D-2006"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:FeesAndCommissionNet>
      |
      |
      |
      |    <!-- Cash Flows, impacts all cash flow statements -->
      |    <gaap:CashAndCashEquivalentsPerCashFlowStatement
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashAndCashEquivalentsPerCashFlowStatement>
      |    <gaap:CashAndCashEquivalentsPerCashFlowStatement
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:CashAndCashEquivalentsPerCashFlowStatement>
      |    <gaap:CashAndCashEquivalentsPerCashFlowStatement
      |            contextRef="I-2005" unitRef="U-Monetary" decimals="INF">-3000
      |    </gaap:CashAndCashEquivalentsPerCashFlowStatement>
      |
      |
      |    <!-- Cash Flows, Direct -->
      |    <gaap:ProceedsFromCollectionOfRevenues
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:ProceedsFromCollectionOfRevenues>
      |    <gaap:PaymentsOfOperatingExpenses
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:PaymentsOfOperatingExpenses>
      |    <gaap:PaymentsOfInterest contextRef="D-2007"
      |                             unitRef="U-Monetary" decimals="INF">1000</gaap:PaymentsOfInterest>
      |    <gaap:CashFlowProvidedByUsedInOperatingActivitiesNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">-1000
      |    </gaap:CashFlowProvidedByUsedInOperatingActivitiesNet>
      |
      |    <gaap:PaymentsForPurchasesOfPropertyPlantAndEquipment
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:PaymentsForPurchasesOfPropertyPlantAndEquipment>
      |    <gaap:ProceedsFromSaleOfPropertyPlantAndEquipment
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:ProceedsFromSaleOfPropertyPlantAndEquipment>
      |    <gaap:CashFlowsProvidedByUsedInInvestingActivitiesNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashFlowsProvidedByUsedInInvestingActivitiesNet>
      |
      |    <gaap:PaymentsOfLongTermDebt contextRef="D-2007"
      |                                 unitRef="U-Monetary" decimals="INF">1000</gaap:PaymentsOfLongTermDebt>
      |    <gaap:ProceedsFromAdditionalLongTermDebt
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:ProceedsFromAdditionalLongTermDebt>
      |    <gaap:CashFlowsProvidedByUsedInFinancingActivitiesNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashFlowsProvidedByUsedInFinancingActivitiesNet>
      |
      |    <gaap:CashFlowNet contextRef="D-2007" unitRef="U-Monetary"
      |                      decimals="INF">-1000</gaap:CashFlowNet>
      |
      |    <gaap:ProceedsFromCollectionOfRevenues
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">6000
      |    </gaap:ProceedsFromCollectionOfRevenues>
      |    <gaap:PaymentsOfOperatingExpenses
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:PaymentsOfOperatingExpenses>
      |    <gaap:PaymentsOfInterest contextRef="D-2006"
      |                             unitRef="U-Monetary" decimals="INF">1000</gaap:PaymentsOfInterest>
      |    <gaap:CashFlowProvidedByUsedInOperatingActivitiesNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:CashFlowProvidedByUsedInOperatingActivitiesNet>
      |
      |    <gaap:PaymentsForPurchasesOfPropertyPlantAndEquipment
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:PaymentsForPurchasesOfPropertyPlantAndEquipment>
      |    <gaap:ProceedsFromSaleOfPropertyPlantAndEquipment
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:ProceedsFromSaleOfPropertyPlantAndEquipment>
      |    <gaap:CashFlowsProvidedByUsedInInvestingActivitiesNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashFlowsProvidedByUsedInInvestingActivitiesNet>
      |
      |    <gaap:PaymentsOfLongTermDebt contextRef="D-2006"
      |                                 unitRef="U-Monetary" decimals="INF">1000</gaap:PaymentsOfLongTermDebt>
      |    <gaap:ProceedsFromAdditionalLongTermDebt
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:ProceedsFromAdditionalLongTermDebt>
      |    <gaap:CashFlowsProvidedByUsedInFinancingActivitiesNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashFlowsProvidedByUsedInFinancingActivitiesNet>
      |
      |    <gaap:CashFlowNet contextRef="D-2006" unitRef="U-Monetary"
      |                      decimals="INF">4000</gaap:CashFlowNet>
      |
      |
      |    <!-- Cash Flows, Indirect -->
      |    <gaap:DepreciationAndAmortization
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:DepreciationAndAmortization>
      |    <gaap:MinorityInterestInNetIncomeLossNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:MinorityInterestInNetIncomeLossNet>
      |    <gaap:OtherAdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperations
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">-2000
      |    </gaap:OtherAdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperations>
      |    <gaap:AdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperations
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:AdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperations>
      |
      |    <gaap:CashFlowProvidedByUsedInOperations
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">-500
      |    </gaap:CashFlowProvidedByUsedInOperations>
      |
      |    <gaap:IncreaseDecreaseInReceivablesNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">3000
      |    </gaap:IncreaseDecreaseInReceivablesNet>
      |    <gaap:IncreaseDecreaseInInventory
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInInventory>
      |    <gaap:IncreaseDecreaseInPrepaidExpenses
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInPrepaidExpenses>
      |    <gaap:IncreaseDecreaseInOtherAssetsCurrent
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:IncreaseDecreaseInOtherAssetsCurrent>
      |    <gaap:IncreaseDecreaseInAccountsPayable
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:IncreaseDecreaseInAccountsPayable>
      |    <gaap:IncreaseDecreaseInAccruedExpenses
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:IncreaseDecreaseInAccruedExpenses>
      |    <gaap:IncreaseDecreaseInOtherLiabilitiesCurrent
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:IncreaseDecreaseInOtherLiabilitiesCurrent>
      |    <gaap:IncreaseDecreaseInWorkingCapital
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1500
      |    </gaap:IncreaseDecreaseInWorkingCapital>
      |    <gaap:ProceedsFromIncomeTaxesRefunded
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:ProceedsFromIncomeTaxesRefunded>
      |    <gaap:PaymentsOfIncomeTaxes contextRef="D-2007"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:PaymentsOfIncomeTaxes>
      |    <gaap:OtherCashFlowFromUsedInOtherOperatingActivities
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherCashFlowFromUsedInOtherOperatingActivities>
      |    <gaap:CashFlowFromUsedInOtherOperatingActivities
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:CashFlowFromUsedInOtherOperatingActivities>
      |
      |
      |    <gaap:DepreciationAndAmortization
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:DepreciationAndAmortization>
      |    <gaap:MinorityInterestInNetIncomeLossNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:MinorityInterestInNetIncomeLossNet>
      |    <gaap:OtherAdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperations
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherAdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperations>
      |    <gaap:AdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperations
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:AdjustmentsToReconcileNetIncomeLossToCashProvidedByUsedInOperations>
      |
      |    <gaap:CashFlowProvidedByUsedInOperations
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:CashFlowProvidedByUsedInOperations>
      |
      |    <gaap:IncreaseDecreaseInReceivablesNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInReceivablesNet>
      |    <gaap:IncreaseDecreaseInInventory
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInInventory>
      |    <gaap:IncreaseDecreaseInPrepaidExpenses
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInPrepaidExpenses>
      |    <gaap:IncreaseDecreaseInOtherAssetsCurrent
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInOtherAssetsCurrent>
      |    <gaap:IncreaseDecreaseInAccountsPayable
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInAccountsPayable>
      |    <gaap:IncreaseDecreaseInAccruedExpenses
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInAccruedExpenses>
      |    <gaap:IncreaseDecreaseInOtherLiabilitiesCurrent
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInOtherLiabilitiesCurrent>
      |    <gaap:IncreaseDecreaseInWorkingCapital
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:IncreaseDecreaseInWorkingCapital>
      |    <gaap:ProceedsFromIncomeTaxesRefunded
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:ProceedsFromIncomeTaxesRefunded>
      |    <gaap:PaymentsOfIncomeTaxes contextRef="D-2006"
      |                                unitRef="U-Monetary" decimals="INF">0</gaap:PaymentsOfIncomeTaxes>
      |    <gaap:OtherCashFlowFromUsedInOtherOperatingActivities
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherCashFlowFromUsedInOtherOperatingActivities>
      |    <gaap:CashFlowFromUsedInOtherOperatingActivities
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CashFlowFromUsedInOtherOperatingActivities>
      |
      |
      |    <!-- Cash Flows, Depository and Lending Institutions -->
      |    <gaap:ProceedsFromCollectionOfInterestIncome
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:ProceedsFromCollectionOfInterestIncome>
      |    <gaap:ProceedsFromFeesAndCommissions
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:ProceedsFromFeesAndCommissions>
      |
      |    <gaap:ProceedsFromCollectionOfInterestIncome
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">5500
      |    </gaap:ProceedsFromCollectionOfInterestIncome>
      |    <gaap:ProceedsFromFeesAndCommissions
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:ProceedsFromFeesAndCommissions>
      |
      |
      |
      |
      |    <!-- Statement of Changes in Equity -->
      |
      |    <!-- Preferred Stock Shares -->
      |    <gaap:PreferredStockShares contextRef="I-2005-PSA"
      |                               unitRef="U-Shares" decimals="INF">6000</gaap:PreferredStockShares>
      |    <gaap:PreferredStockShares contextRef="I-2006-PSA"
      |                               unitRef="U-Shares" decimals="INF">6000</gaap:PreferredStockShares>
      |    <gaap:PreferredStockShares contextRef="I-2007-PSA"
      |                               unitRef="U-Shares" decimals="INF">6000</gaap:PreferredStockShares>
      |
      |    <gaap:PreferredStockSharesAdditionalSharesIssued
      |            contextRef="D-2006-PSA" unitRef="U-Shares" decimals="INF">7000
      |    </gaap:PreferredStockSharesAdditionalSharesIssued>
      |    <gaap:PreferredStockSharesBuybacks
      |            contextRef="D-2006-PSA" unitRef="U-Shares" decimals="INF">6000
      |    </gaap:PreferredStockSharesBuybacks>
      |    <gaap:OtherIncreaseDecreaseInPreferredStockShares
      |            contextRef="D-2006-PSA" unitRef="U-Shares" decimals="INF">-1000
      |    </gaap:OtherIncreaseDecreaseInPreferredStockShares>
      |    <gaap:PreferredStockSharesPeriodIncreaseDecrease
      |            contextRef="D-2006-PSA" unitRef="U-Shares" decimals="INF">0
      |    </gaap:PreferredStockSharesPeriodIncreaseDecrease>
      |
      |    <gaap:PreferredStockSharesAdditionalSharesIssued
      |            contextRef="D-2007-PSA" unitRef="U-Shares" decimals="INF">7000
      |    </gaap:PreferredStockSharesAdditionalSharesIssued>
      |    <gaap:PreferredStockSharesBuybacks
      |            contextRef="D-2007-PSA" unitRef="U-Shares" decimals="INF">6000
      |    </gaap:PreferredStockSharesBuybacks>
      |    <gaap:OtherIncreaseDecreaseInPreferredStockShares
      |            contextRef="D-2007-PSA" unitRef="U-Shares" decimals="INF">-1000
      |    </gaap:OtherIncreaseDecreaseInPreferredStockShares>
      |    <gaap:PreferredStockSharesPeriodIncreaseDecrease
      |            contextRef="D-2007-PSA" unitRef="U-Shares" decimals="INF">0
      |    </gaap:PreferredStockSharesPeriodIncreaseDecrease>
      |
      |    <!-- Common Stock Shares -->
      |    <gaap:CommonStockShares contextRef="I-2005-CSA"
      |                            unitRef="U-Shares" decimals="INF">6000</gaap:CommonStockShares>
      |    <gaap:CommonStockShares contextRef="I-2006-CSA"
      |                            unitRef="U-Shares" decimals="INF">6000</gaap:CommonStockShares>
      |    <gaap:CommonStockShares contextRef="I-2007-CSA"
      |                            unitRef="U-Shares" decimals="INF">6000</gaap:CommonStockShares>
      |
      |    <gaap:CommonStockSharesAdditionalSharesIssued
      |            contextRef="D-2006-CSA" unitRef="U-Shares" decimals="INF">5000
      |    </gaap:CommonStockSharesAdditionalSharesIssued>
      |    <gaap:CommonStockSharesOptionsExercised
      |            contextRef="D-2006-CSA" unitRef="U-Shares" decimals="INF">6000
      |    </gaap:CommonStockSharesOptionsExercised>
      |    <gaap:OtherIncreaseDecreaseInCommonStockShares
      |            contextRef="D-2006-CSA" unitRef="U-Shares" decimals="INF">-11000
      |    </gaap:OtherIncreaseDecreaseInCommonStockShares>
      |    <gaap:CommonStockSharesPeriodIncreaseDecrease
      |            contextRef="D-2006-CSA" unitRef="U-Shares" decimals="INF">0
      |    </gaap:CommonStockSharesPeriodIncreaseDecrease>
      |
      |    <gaap:CommonStockSharesAdditionalSharesIssued
      |            contextRef="D-2007-CSA" unitRef="U-Shares" decimals="INF">5000
      |    </gaap:CommonStockSharesAdditionalSharesIssued>
      |    <gaap:CommonStockSharesOptionsExercised
      |            contextRef="D-2007-CSA" unitRef="U-Shares" decimals="INF">6000
      |    </gaap:CommonStockSharesOptionsExercised>
      |    <gaap:OtherIncreaseDecreaseInCommonStockShares
      |            contextRef="D-2007-CSA" unitRef="U-Shares" decimals="INF">-11000
      |    </gaap:OtherIncreaseDecreaseInCommonStockShares>
      |    <gaap:CommonStockSharesPeriodIncreaseDecrease
      |            contextRef="D-2007-CSA" unitRef="U-Shares" decimals="INF">0
      |    </gaap:CommonStockSharesPeriodIncreaseDecrease>
      |
      |
      |    <!-- Preferred Stock Amount -->
      |    <gaap:PreferredStockIssued contextRef="D-2006"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:PreferredStockIssued>
      |    <gaap:PreferredStockBuybacks contextRef="D-2006"
      |                                 unitRef="U-Monetary" decimals="INF">1000</gaap:PreferredStockBuybacks>
      |    <gaap:OtherIncreaseDecreaseInPreferredStock
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherIncreaseDecreaseInPreferredStock>
      |    <gaap:PreferredStockPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:PreferredStockPeriodIncreaseDecrease>
      |
      |    <gaap:PreferredStockIssued contextRef="D-2007"
      |                               unitRef="U-Monetary" decimals="INF">1000</gaap:PreferredStockIssued>
      |    <gaap:PreferredStockBuybacks contextRef="D-2007"
      |                                 unitRef="U-Monetary" decimals="INF">1000</gaap:PreferredStockBuybacks>
      |    <gaap:OtherIncreaseDecreaseInPreferredStock
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherIncreaseDecreaseInPreferredStock>
      |    <gaap:PreferredStockPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:PreferredStockPeriodIncreaseDecrease>
      |
      |    <!-- Common Stock Amount -->
      |    <gaap:CommonStockIssued contextRef="D-2006"
      |                            unitRef="U-Monetary" decimals="INF">1000</gaap:CommonStockIssued>
      |    <gaap:CommonStockOptionsExercised
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:CommonStockOptionsExercised>
      |    <gaap:OtherIncreaseDecreaseInCommonStock
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">-2000
      |    </gaap:OtherIncreaseDecreaseInCommonStock>
      |    <gaap:CommonStockPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CommonStockPeriodIncreaseDecrease>
      |
      |    <gaap:CommonStockIssued contextRef="D-2007"
      |                            unitRef="U-Monetary" decimals="INF">1000</gaap:CommonStockIssued>
      |    <gaap:CommonStockOptionsExercised
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:CommonStockOptionsExercised>
      |    <gaap:OtherIncreaseDecreaseInCommonStock
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">-2000
      |    </gaap:OtherIncreaseDecreaseInCommonStock>
      |    <gaap:CommonStockPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:CommonStockPeriodIncreaseDecrease>
      |
      |
      |    <!-- Additional Paid in Capital -->
      |    <gaap:AdditionalPaidInCapitalAddedFromIssuanceOfCommonStock
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AdditionalPaidInCapitalAddedFromIssuanceOfCommonStock>
      |    <gaap:AdditionalPaidInCapitalRemovedFromBuybacksOfPreferredStock
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AdditionalPaidInCapitalRemovedFromBuybacksOfPreferredStock>
      |    <gaap:OtherIncreaseDecreaseInAdditionalPaidInCapital
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherIncreaseDecreaseInAdditionalPaidInCapital>
      |    <gaap:AdditionalPaidInCapitalPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AdditionalPaidInCapitalPeriodIncreaseDecrease>
      |
      |    <gaap:AdditionalPaidInCapitalAddedFromIssuanceOfCommonStock
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AdditionalPaidInCapitalAddedFromIssuanceOfCommonStock>
      |    <gaap:AdditionalPaidInCapitalRemovedFromBuybacksOfPreferredStock
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AdditionalPaidInCapitalRemovedFromBuybacksOfPreferredStock>
      |    <gaap:OtherIncreaseDecreaseInAdditionalPaidInCapital
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherIncreaseDecreaseInAdditionalPaidInCapital>
      |    <gaap:AdditionalPaidInCapitalPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AdditionalPaidInCapitalPeriodIncreaseDecrease>
      |
      |
      |    <!-- Retained Earnings -->
      |    <gaap:OtherIncreaseDecreaseInRetainedEarningsAccumulatedLosses
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">6000
      |    </gaap:OtherIncreaseDecreaseInRetainedEarningsAccumulatedLosses>
      |    <gaap:RetainedEarningsAccumulatedLossesPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:RetainedEarningsAccumulatedLossesPeriodIncreaseDecrease>
      |
      |    <!-- Note the IDs which indicate restatement. -->
      |    <gaap:RetainedEarningsAccumulatedLosses
      |            contextRef="I-2006-OrigionallyReported" unitRef="U-Monetary" decimals="INF">
      |        1000</gaap:RetainedEarningsAccumulatedLosses>
      |
      |    <!-- What should the period of the restatement adjustment concepts be? -->
      |    <gaap:RetainedEarningsAccumulatedLossesPriorPeriodErrors
      |            id="Restated-01" contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:RetainedEarningsAccumulatedLossesPriorPeriodErrors>
      |    <gaap:RetainedEarningsAccumulatedLossesChangesInAccountingPolicies
      |            id="Restated-02" contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:RetainedEarningsAccumulatedLossesChangesInAccountingPolicies>
      |    <gaap:RetainedEarningsAccumulatedLossesPriorPeriodAdjustments
      |            id="Restated-03" contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:RetainedEarningsAccumulatedLossesPriorPeriodAdjustments>
      |
      |    <gaap:OtherIncreaseDecreaseInRetainedEarningsAccumulatedLosses
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:OtherIncreaseDecreaseInRetainedEarningsAccumulatedLosses>
      |    <gaap:RetainedEarningsAccumulatedLossesPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:RetainedEarningsAccumulatedLossesPeriodIncreaseDecrease>
      |
      |
      |    <!-- Equity, Total -->
      |    <gaap:DividendsPaid contextRef="D-2006" unitRef="U-Monetary"
      |                        decimals="INF">1000</gaap:DividendsPaid>
      |    <gaap:PreferredStockAndAdditionalPaidInCapitalIssuanceOfPreferredStock
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:PreferredStockAndAdditionalPaidInCapitalIssuanceOfPreferredStock>
      |    <gaap:CommonStockAndAdditionalPaidInCapitalIssuanceOfCommonStock
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:CommonStockAndAdditionalPaidInCapitalIssuanceOfCommonStock>
      |    <gaap:PreferredStockAndAdditionalPaidInCapitalRemovedBuybacksOfPreferredStock
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:PreferredStockAndAdditionalPaidInCapitalRemovedBuybacksOfPreferredStock>
      |    <gaap:OtherIncreaseDecreaseInEquity
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">6000
      |    </gaap:OtherIncreaseDecreaseInEquity>
      |    <gaap:EquityPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">3000
      |    </gaap:EquityPeriodIncreaseDecrease>
      |
      |
      |    <gaap:DividendsPaid contextRef="D-2007" unitRef="U-Monetary"
      |                        decimals="INF">1000</gaap:DividendsPaid>
      |    <gaap:PreferredStockAndAdditionalPaidInCapitalIssuanceOfPreferredStock
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:PreferredStockAndAdditionalPaidInCapitalIssuanceOfPreferredStock>
      |    <gaap:CommonStockAndAdditionalPaidInCapitalIssuanceOfCommonStock
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:CommonStockAndAdditionalPaidInCapitalIssuanceOfCommonStock>
      |    <gaap:PreferredStockAndAdditionalPaidInCapitalRemovedBuybacksOfPreferredStock
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:PreferredStockAndAdditionalPaidInCapitalRemovedBuybacksOfPreferredStock>
      |    <gaap:OtherIncreaseDecreaseInEquity
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:OtherIncreaseDecreaseInEquity>
      |    <gaap:EquityPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:EquityPeriodIncreaseDecrease>
      |
      |
      |
      |""".stripMargin

  private val xmlStringPart6 =
    """    <!-- Policies and Disclosures -->
      |
      |    <gaap:CashAndCashEquivalentsPolicy
      |            contextRef="D-2007">Sed mauris. Nulla facilisi. Fusce tristique posuere ipsum.
      |        Nulla facilisi. Aliquam viverra risus vitae ante. Sed rhoncus mi in
      |        wisi. Nullam nibh dui, molestie vitae, imperdiet non, ornare at, elit.
      |        Aenean nec justo. Vestibulum ante ipsum primis in faucibus orci luctus
      |        et ultrices posuere cubilia Curae; Duis sodales.
      |    </gaap:CashAndCashEquivalentsPolicy>
      |    <gaap:ReceivablesPolicy contextRef="D-2007">Etiam viverra
      |        neque non sem. Nullam lacinia sem. Duis sapien diam, dapibus sed,
      |        dictum quis, interdum ac, erat. Suspendisse urna. Proin non mauris.
      |        Proin sed odio. Phasellus sagittis orci quis orci. Aliquam erat
      |        volutpat. Mauris commodo ultrices elit. Sed id orci. In ultrices urna
      |        volutpat wisi.</gaap:ReceivablesPolicy>
      |
      |    <gaap:InventoryPolicy contextRef="D-2007">Proin elit sem,
      |        ornare non, ullamcorper vel, sollicitudin a, lacus. Mauris tincidunt
      |        cursus est. Nulla sit amet nibh. Sed elementum feugiat augue. Nam non
      |        tortor non leo porta bibendum. Morbi eu pede. In eu erat et est
      |        feugiat fermentum. Praesent accumsan. Nulla convallis, lorem nec
      |        aliquet dapibus, libero felis sagittis augue, ut adipiscing nisl eros
      |        in quam. Fusce eleifend. Sed justo nibh, placerat a, malesuada nec,
      |        condimentum ac, magna.</gaap:InventoryPolicy>
      |    <gaap:InventoryValuationMethod
      |            contextRef="D-2007">Cost</gaap:InventoryValuationMethod>
      |    <gaap:InventoryCostMethod contextRef="D-2007">Suspendisse
      |        vestibulum augue eu justo. Pellentesque habitant morbi tristique
      |        senectus et netus et malesuada fames ac turpis egestas. Fusce suscipit
      |        pede ut erat. Sed rutrum. Pellentesque eget mi. In enim. Praesent nec
      |        eros. Aenean convallis sem condimentum justo. Integer quam. Ut
      |        ultrices, purus sit amet pretium bibendum, risus est facilisis purus,
      |        quis suscipit leo ipsum fringilla ligula. Lum sociis natoque penatibus
      |        et magnis dis parturient montes, nascetur ridiculus mus. Aliquam erat
      |        volutpat.</gaap:InventoryCostMethod>
      |    <gaap:InventoryDisclosures contextRef="D-2007">Proin
      |        elit sem, ornare non, ullamcorper vel, sollicitudin a, lacus. Mauris
      |        tincidunt cursus est. Nulla sit amet nibh. Sed elementum feugiat
      |        augue. Nam non tortor non leo porta bibendum. Morbi eu pede. In eu
      |        erat et est feugiat fermentum. Praesent accumsan. Nulla convallis,
      |        lorem nec aliquet dapibus, libero felis sagittis augue, ut adipiscing
      |        nisl eros in quam. Fusce eleifend. Sed justo nibh, placerat a,
      |        malesuada nec, condimentum ac, magna.</gaap:InventoryDisclosures>
      |    <gaap:InventoryDescriptionOfComponents
      |            contextRef="D-2007">Phasellus venenatis enim tempus elit. Ut tristique, leo sed
      |        consequat consequat, lorem ipsum pretium odio, sit amet ultricies dui
      |        nulla ac wisi: (a) malesuada fames ac turpis, (b) eget mi, (c)
      |        convallis sem condimentum justo.
      |    </gaap:InventoryDescriptionOfComponents>
      |    <gaap:InventoryDescriptionOfCalculationOfNetRealizableValue
      |            contextRef="D-2007">Duis fermentum. Nullam dui orci, scelerisque porttitor,
      |        volutpat a, porttitor a, enim. Sed lobortis.
      |    </gaap:InventoryDescriptionOfCalculationOfNetRealizableValue>
      |    <gaap:PrepaidExpensesPolicy contextRef="D-2007">Donec
      |        pulvinar nonummy erat. In vel justo at urna rutrum ultrices. Cras
      |        consectetuer orci non lorem. Vestibulum bibendum aliquet augue. Proin
      |        venenatis ultricies risus. Aliquam erat volutpat. Donec tempus ligula
      |        a risus. Nam porttitor. Aliquam mattis erat aliquet mi. Sed commodo,
      |        ante adipiscing porttitor sollicitudin, sapien wisi ullamcorper ante,
      |        nec ultricies orci tellus id justo.</gaap:PrepaidExpensesPolicy>
      |
      |    <!-- FIX: Need to do for each class, use class properties -->
      |    <gaap:PropertyPlantAndEquipmentPolicy
      |            contextRef="D-2007">Proin feugiat lobortis mi. Nunc congue. Fusce venenatis.
      |        Maecenas tincidunt, ipsum in fringilla hendrerit, dolor metus eleifend
      |        neque, vel tincidunt mi nunc a purus. Curabitur porta dapibus odio.
      |        Proin commodo. Vestibulum odio mi, varius sit amet, adipiscing id,
      |        semper non, elit. Integer hendrerit placerat sem. Duis adipiscing urna
      |        ac wisi. Cras sapien.</gaap:PropertyPlantAndEquipmentPolicy>
      |    <gaap:PropertyPlantAndEquipmentMeasurementBasis
      |            contextRef="D-2007">Nam rhoncus mi. Nunc eu dui non mauris interdum tincidunt.
      |        Sed magna felis, accumsan a, fermentum quis, varius sed, ipsum. Nullam
      |        leo.</gaap:PropertyPlantAndEquipmentMeasurementBasis>
      |
      |    <gaap:MeasurementBasisBuildings
      |            contextRef="D-2007">Curabitur fermentum mattis eros. Pellentesque pede felis,
      |        rhoncus accumsan, luctus ut, facilisis vitae, tortor.
      |    </gaap:MeasurementBasisBuildings>
      |    <gaap:MeasurementBasisFurnitureAndFixtures
      |            contextRef="D-2007">Praesent id mauris. Sed dapibus dui quis lectus.
      |    </gaap:MeasurementBasisFurnitureAndFixtures>
      |    <gaap:MeasurementBasisOtherPropertyPlantAndEquipment
      |            contextRef="D-2007">In vel justo at urna rutrum ultrices.
      |    </gaap:MeasurementBasisOtherPropertyPlantAndEquipment>
      |
      |    <gaap:PropertyPlantAndEquipmentDepreciationMethod
      |            contextRef="D-2007">Sed eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum.
      |        Sed in purus.</gaap:PropertyPlantAndEquipmentDepreciationMethod>
      |
      |    <gaap:DepreciationMethodBuildings
      |            contextRef="D-2007">Duis metus. Donec pulvinar nonummy erat. In vel justo at
      |        urna rutrum ultrices. Cras consectetuer orci non lorem.
      |    </gaap:DepreciationMethodBuildings>
      |    <gaap:DepreciationMethodFurnitureAndFixtures
      |            contextRef="D-2007">Sed eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum.
      |        Sed in purus. Class aptent taciti sociosqu ad litora torquent per
      |        conubia nostra, per inceptos hymenaeos.
      |    </gaap:DepreciationMethodFurnitureAndFixtures>
      |    <gaap:DepreciationMethodOtherPropertyPlantAndEquipment
      |            contextRef="D-2007">Sed eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum.
      |        Sed in purus. Class aptent taciti sociosqu ad litora torquent per
      |        conubia nostra, per inceptos hymenaeos.
      |    </gaap:DepreciationMethodOtherPropertyPlantAndEquipment>
      |
      |
      |    <gaap:PropertyPlantAndEquipmentEstimatedUsefulLife
      |            contextRef="D-2007">Sed eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum.
      |        Sed in purus. Class aptent taciti sociosqu ad litora torquent per
      |        conubia nostra, per inceptos hymenaeos.
      |    </gaap:PropertyPlantAndEquipmentEstimatedUsefulLife>
      |
      |    <gaap:EstimatedUsefulLifeBuildings
      |            contextRef="D-2007">Sed eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum.
      |        Sed in purus. Class aptent taciti sociosqu ad litora torquent per
      |        conubia nostra, per inceptos hymenaeos.
      |    </gaap:EstimatedUsefulLifeBuildings>
      |    <gaap:EstimatedUsefulLifeFurnitureAndFixtures
      |            contextRef="D-2007">Curabitur fermentum mattis eros. Pellentesque pede felis,
      |        rhoncus accumsan, luctus ut, facilisis vitae, tortor.
      |    </gaap:EstimatedUsefulLifeFurnitureAndFixtures>
      |    <gaap:EstimatedUsefulLifeOtherPropertyPlantAndEquipment
      |            contextRef="D-2007">Sed eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum.
      |        Sed in purus. Class aptent taciti sociosqu ad litora torquent per
      |        conubia nostra, per inceptos hymenaeos.
      |    </gaap:EstimatedUsefulLifeOtherPropertyPlantAndEquipment>
      |
      |    <gaap:OtherAssetsPolicy contextRef="D-2007">These are the
      |        other assets policies. Blah blah blah blah.</gaap:OtherAssetsPolicy>
      |
      |    <gaap:PayablesAndAccrualsPolicy
      |            contextRef="D-2007">Fusce vitae mi. Sed dapibus venenatis ipsum. Sed in purus.
      |        Class aptent taciti sociosqu ad litora torquent per conubia nostra,
      |        per inceptos hymenaeos.</gaap:PayablesAndAccrualsPolicy>
      |    <gaap:AccountsPayablePolicy contextRef="D-2007">Fusce
      |        vitae mi. Sed dapibus venenatis ipsum. Sed in purus. Class aptent
      |        taciti sociosqu ad litora torquent per conubia nostra, per inceptos
      |        hymenaeos.</gaap:AccountsPayablePolicy>
      |    <gaap:AccruedInterestPolicy contextRef="D-2007">Fusce
      |        vitae mi. Sed dapibus venenatis ipsum. Sed in purus. Class aptent
      |        taciti sociosqu ad litora torquent per conubia nostra, per inceptos
      |        hymenaeos. Duis metus. Donec pulvinar nonummy erat. In vel justo at
      |        urna rutrum ultrices. Cras consectetuer orci non lorem.
      |    </gaap:AccruedInterestPolicy>
      |    <gaap:AccruedExpensesPolicy contextRef="D-2007">Curabitur
      |        fermentum mattis eros. Pellentesque pede felis, rhoncus accumsan,
      |        luctus ut, facilisis vitae, tortor.</gaap:AccruedExpensesPolicy>
      |    <gaap:OtherPayablesAndAccrualsPolicy
      |            contextRef="D-2007">Etiam porttitor. Ut venenatis, velit a accumsan interdum,
      |        odio metus mollis mauris, non pharetra augue arcu eu felis. Ut eget
      |        felis. Mauris leo nulla, sodales et, pharetra quis, fermentum nec,
      |        diam. Maecenas eget metus. Donec ultricies risus non dui. Ut ipsum
      |        mauris, pellentesque ut, tristique eget, fringilla id, ante.
      |        Vestibulum ante ipsum primis in faucibus orci luctus et ultrices
      |        posuere cubilia Curae; Duis erat. Curabitur non magna. Nullam wisi
      |        lectus, rutrum sit amet, fermentum et, venenatis hendrerit, magna.
      |    </gaap:OtherPayablesAndAccrualsPolicy>
      |    <gaap:DebtPolicy contextRef="D-2007">Etiam viverra neque non
      |        sem. Nullam lacinia sem. Duis sapien diam, dapibus sed, dictum quis,
      |        interdum ac, erat. Suspendisse urna. Proin non mauris. Proin sed odio.
      |    </gaap:DebtPolicy>
      |    <gaap:LongTermDebtPolicy contextRef="D-2007">Proin
      |        feugiat lobortis mi. Nunc congue. Fusce venenatis. Maecenas tincidunt,
      |        ipsum in fringilla hendrerit, dolor metus eleifend neque, vel
      |        tincidunt mi nunc a purus. Curabitur porta dapibus odio. Proin
      |        commodo. Vestibulum odio mi, varius sit amet, adipiscing id, semper
      |        non, elit. Integer hendrerit placerat sem. Duis adipiscing urna ac
      |        wisi. Cras sapien. Sed faucibus consequat pede.
      |    </gaap:LongTermDebtPolicy>
      |    <gaap:OtherLiabilitiesPolicy contextRef="D-2007">Etiam
      |        porttitor. Ut venenatis, velit a accumsan interdum, odio metus mollis
      |        mauris, non pharetra augue arcu eu felis. Ut eget felis. Mauris leo
      |        nulla, sodales et, pharetra quis, fermentum nec, diam. Maecenas eget
      |        metus. Donec ultricies risus non dui. Ut ipsum mauris, pellentesque
      |        ut, tristique eget, fringilla id, ante. Vestibulum ante ipsum primis
      |        in faucibus orci luctus et ultrices posuere cubilia Curae; Duis erat.
      |        Curabitur non magna. Nullam wisi lectus, rutrum sit amet, fermentum
      |        et, venenatis hendrerit, magna.</gaap:OtherLiabilitiesPolicy>
      |
      |    <gaap:EquityPolicies contextRef="D-2007">Proin feugiat
      |        lobortis mi. Nunc congue. Fusce venenatis. Maecenas tincidunt, ipsum
      |        in fringilla hendrerit, dolor metus eleifend neque, vel tincidunt mi
      |        nunc a purus. Curabitur porta dapibus odio. Proin commodo. Vestibulum
      |        odio mi, varius sit amet, adipiscing id, semper non, elit. Integer
      |        hendrerit placerat sem. Duis adipiscing urna ac wisi. Cras sapien. Sed
      |        faucibus consequat pede.</gaap:EquityPolicies>
      |    <gaap:CommonStockPolicies contextRef="D-2007">Etiam
      |        viverra neque non sem. Nullam lacinia sem. Duis sapien diam, dapibus
      |        sed, dictum quis, interdum ac, erat. Suspendisse urna. Proin non
      |        mauris. Proin sed odio.</gaap:CommonStockPolicies>
      |    <gaap:PreferredStockPolicies contextRef="D-2007">Sed
      |        eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum. Sed in purus.
      |        Class aptent taciti sociosqu ad litora torquent per conubia nostra,
      |        per inceptos hymenaeos.</gaap:PreferredStockPolicies>
      |    <gaap:AdditionalPaidInCapitalPolicies
      |            contextRef="D-2007">Etiam porttitor. Ut venenatis, velit a accumsan interdum,
      |        odio metus mollis mauris, non pharetra augue arcu eu felis. Ut eget
      |        felis. Mauris leo nulla, sodales et, pharetra quis, fermentum nec,
      |        diam. Maecenas eget metus. Donec ultricies risus non dui. Ut ipsum
      |        mauris, pellentesque ut, tristique eget, fringilla id, ante.
      |        Vestibulum ante ipsum primis in faucibus orci luctus et ultrices
      |        posuere cubilia Curae; Duis erat. Curabitur non magna. Nullam wisi
      |        lectus, rutrum sit amet, fermentum et, venenatis hendrerit, magna.
      |    </gaap:AdditionalPaidInCapitalPolicies>
      |    <gaap:RetainedEarningsAccumulatedLossesPolicies
      |            contextRef="D-2007">Sed eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum.
      |        Sed in purus. Class aptent taciti sociosqu ad litora torquent per
      |        conubia nostra, per inceptos hymenaeos.
      |    </gaap:RetainedEarningsAccumulatedLossesPolicies>
      |
      |    <gaap:IncomeTaxesPolicy contextRef="D-2007">Fusce
      |        venenatis. Maecenas tincidunt, ipsum in fringilla hendrerit, dolor
      |        metu adipiscing urna ac wisi. Cras sapien. Sed faucibus consequat
      |        pede.</gaap:IncomeTaxesPolicy>
      |
      |    <gaap:OtherAccountingPolicies contextRef="D-2007">Proin
      |        feugiat lobortis mi. Nunc congue. Fusce venenatis. Maecenas tincidunt,
      |        ipsum in fringilla hendrerit, dolor metus eleifend neque, vel
      |        tincidunt mi nunc a purus. Curabitur porta dapibus odio. Proin
      |        commodo. Vestibulum odio mi, varius sit amet, adipiscing id, semper
      |        non, elit. Integer hendrerit placerat sem. Duis adipiscing urna ac
      |        wisi. Cras sapien. Sed faucibus consequat pede.
      |    </gaap:OtherAccountingPolicies>
      |
      |
      |    <!-- Cash and Cash Equivalents -->
      |    <gaap:CashAndCashEquivalentsTextBlock
      |            contextRef="D-2007">Etiam viverra neque non sem. Nullam lacinia sem. Duis
      |        sapien diam, dapibus sed, dictum quis, interdum ac, erat. Suspendisse
      |        urna. Proin non mauris. Proin sed odio.
      |    </gaap:CashAndCashEquivalentsTextBlock>
      |
      |    <gaap:CashAndCashEquivalentsDisclosures
      |            contextRef="D-2007">Etiam porttitor. Ut venenatis, velit a accumsan interdum,
      |        odio metus mollis mauris, non pharetra augue arcu eu felis. Ut eget
      |        felis. Mauris leo nulla, sodales et, pharetra quis, fermentum nec,
      |        diam. Maecenas eget metus. Donec ultricies risus non dui. Ut ipsum
      |        mauris, pellentesque ut, tristique eget, fringilla id, ante.
      |        Vestibulum ante ipsum primis in faucibus orci luctus et ultrices
      |        posuere cubilia Curae; Duis erat. Curabitur non magna. Nullam wisi
      |        lectus, rutrum sit amet, fermentum et, venenatis hendrerit, magna.
      |    </gaap:CashAndCashEquivalentsDisclosures>
      |
      |    <gaap:CashUnrestricted contextRef="I-2007"
      |                           unitRef="U-Monetary" decimals="INF">250</gaap:CashUnrestricted>
      |    <gaap:CashRestricted contextRef="I-2007" unitRef="U-Monetary"
      |                         decimals="INF">250</gaap:CashRestricted>
      |    <gaap:ShortTermInvestments contextRef="I-2007"
      |                               unitRef="U-Monetary" decimals="INF">250</gaap:ShortTermInvestments>
      |    <gaap:OtherCashAndCashEquivalents
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:OtherCashAndCashEquivalents>
      |
      |    <gaap:CashUnrestricted contextRef="I-2006"
      |                           unitRef="U-Monetary" decimals="INF">250</gaap:CashUnrestricted>
      |    <gaap:CashRestricted contextRef="I-2006" unitRef="U-Monetary"
      |                         decimals="INF">250</gaap:CashRestricted>
      |    <gaap:ShortTermInvestments contextRef="I-2006"
      |                               unitRef="U-Monetary" decimals="INF">250</gaap:ShortTermInvestments>
      |    <gaap:OtherCashAndCashEquivalents
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:OtherCashAndCashEquivalents>
      |
      |
      |
      |    <!-- Receivables -->
      |    <gaap:ReceivablesDisclosures contextRef="D-2007">Sed
      |        eu nibh. Fusce vitae mi. Sed dapibus venenatis ipsum. Sed in purus.
      |        Class aptent taciti sociosqu ad litora torquent per conubia nostra,
      |        per inceptos hymenaeos.</gaap:ReceivablesDisclosures>
      |    <gaap:NotesReceivablesDisclosures
      |            contextRef="D-2007">Etiam ipsum orci, gravida nec, feugiat ut, malesuada quis,
      |        mauris. Etiam porttitor. Ut venenatis, velit a accumsan interdum, odio
      |        metus mollis mauris, non pharetra augue arcu eu felis. Ut eget felis.
      |        Mauris leo nulla, sodales et, pharetra quis, fermentum nec, diam.
      |        Maecenas eget metus. Donec ultricies risus non dui. Ut ipsum mauris,
      |        pellentesque ut, tristique eget, fringilla id, ante.
      |    </gaap:NotesReceivablesDisclosures>
      |    <gaap:OtherReceivablesDisclosures
      |            contextRef="D-2007">Integer hendrerit placerat sem. Duis adipiscing urna ac
      |        wisi. Cras sapien. Sed faucibus consequat pede. Proin id nibh.
      |    </gaap:OtherReceivablesDisclosures>
      |
      |
      |    <gaap:TradeReceivablesNetCurrent
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:TradeReceivablesNetCurrent>
      |    <gaap:FinanceLeaseReceivablesNetCurrent
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">400
      |    </gaap:FinanceLeaseReceivablesNetCurrent>
      |    <gaap:OtherReceivablesNetCurrent
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">100
      |    </gaap:OtherReceivablesNetCurrent>
      |
      |    <gaap:TradeReceivablesNetCurrent
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:TradeReceivablesNetCurrent>
      |    <gaap:FinanceLeaseReceivablesNetCurrent
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">400
      |    </gaap:FinanceLeaseReceivablesNetCurrent>
      |    <gaap:OtherReceivablesNetCurrent
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">100
      |    </gaap:OtherReceivablesNetCurrent>
      |
      |
      |    <gaap:ReceivablesGross contextRef="I-2007"
      |                           unitRef="U-Monetary" decimals="INF">2000</gaap:ReceivablesGross>
      |    <gaap:AllowanceForDoubtfulAccountsReceivables
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AllowanceForDoubtfulAccountsReceivables>
      |
      |    <gaap:ReceivablesGross contextRef="I-2006"
      |                           unitRef="U-Monetary" decimals="INF">2000</gaap:ReceivablesGross>
      |    <gaap:AllowanceForDoubtfulAccountsReceivables
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:AllowanceForDoubtfulAccountsReceivables>
      |
      |    <gaap:ReceivablesNet contextRef="I-2007" unitRef="U-Monetary"
      |                         decimals="INF">1000</gaap:ReceivablesNet>
      |    <gaap:ReceivablesNetNoncurrent
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:ReceivablesNetNoncurrent>
      |
      |    <gaap:ReceivablesNet contextRef="I-2006" unitRef="U-Monetary"
      |                         decimals="INF">1000</gaap:ReceivablesNet>
      |    <gaap:ReceivablesNetNoncurrent
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:ReceivablesNetNoncurrent>
      |
      |
      |    <!-- Inventory -->
      |    <gaap:InventoryDisclosures contextRef="D-2007">Integer
      |        hendrerit placerat sem. Duis adipiscing urna ac wisi. Cras sapien. Sed
      |        faucibus consequat pede. Proin id nibh. </gaap:InventoryDisclosures>
      |    <gaap:InventoryDescriptionOfComponents
      |            contextRef="D-2007">Phasellus venenatis enim tempus elit. Ut tristique, leo sed
      |        consequat consequat, lorem ipsum pretium odio, sit amet ultricies dui
      |        nulla ac wisi. Praesent wisi erat, placerat eu, posuere tincidunt,
      |        laoreet ac, nibh. Etiam ut augue. Maecenas eget lectus vel metus
      |        vehicula consectetuer. Aliquam erat volutpat. In faucibus. Donec ac
      |        arcu. Curabitur purus risus, hendrerit lacinia, lacinia eget, placerat
      |        quis, turpis. Nulla aliquam ligula at libero.
      |    </gaap:InventoryDescriptionOfComponents>
      |    <gaap:InventoryDescriptionOfCalculationOfNetRealizableValue
      |            contextRef="D-2007">Ut tristique, leo sed consequat consequat, lorem ipsum
      |        pretium odio, sit amet ultricies dui nulla ac wisi. Praesent wisi
      |        erat, placerat eu, posuere tincidunt, laoreet ac, nibh. Etiam ut
      |        augue. Maecenas eget lectus vel metus vehicula consectetuer. Aliquam
      |        erat volutpat. In faucibus. Donec ac arcu. Curabitur purus risus,
      |        hendrerit lacinia, lacinia eget, placerat quis, turpis. Nulla aliquam
      |        ligula at libero.
      |    </gaap:InventoryDescriptionOfCalculationOfNetRealizableValue>
      |
      |
      |
      |    <!-- Prepaid Expenses -->
      |    <gaap:PrepaidExpensesDisclosures
      |            contextRef="D-2007">Etiam ipsum orci, gravida nec, feugiat ut, malesuada quis,
      |        mauris. Etiam porttitor. Ut venenatis, velit a accumsan interdum, odio
      |        metus mollis mauris, non pharetra augue arcu eu felis. Ut eget felis.
      |        Mauris leo nulla, sodales et, pharetra quis, fermentum nec, diam.
      |        Maecenas eget metus. Donec ultricies risus non dui. Ut ipsum mauris,
      |        pellentesque ut, tristique eget, fringilla id, ante. Vestibulum ante
      |        ipsum primis in faucibus orci luctus et ultrices posuere cubilia
      |        Curae; Duis erat. Curabitur non magna. Nullam wisi lectus, rutrum sit
      |        amet, fermentum et, venenatis hendrerit, magna. Suspendisse convallis
      |        urna elementum elit.</gaap:PrepaidExpensesDisclosures>
      |
      |
      |    <!-- Property, Plant, and Equipment -->
      |
      |    <!-- FIX: Need to do for each class, use class properties -->
      |    <gaap:PropertyPlantAndEquipmentMeasurementBasis
      |            contextRef="D-2007-PPE-Land">This is information about the measurement basis for LAND.
      |    </gaap:PropertyPlantAndEquipmentMeasurementBasis>
      |    <gaap:PropertyPlantAndEquipmentDepreciationMethod
      |            contextRef="D-2007-PPE-Land">This is information relating to depreciation method for
      |        LAND.</gaap:PropertyPlantAndEquipmentDepreciationMethod>
      |    <gaap:PropertyPlantAndEquipmentEstimatedUsefulLife
      |            contextRef="D-2007-PPE-Land">This is information relating to the estimated useful life
      |        for LAND.</gaap:PropertyPlantAndEquipmentEstimatedUsefulLife>
      |
      |    <gaap:PropertyPlantAndEquipmentMeasurementBasis
      |            contextRef="D-2007-PPE-Buildings">This is information about the measurement basis for
      |        Buildings.</gaap:PropertyPlantAndEquipmentMeasurementBasis>
      |    <gaap:PropertyPlantAndEquipmentDepreciationMethod
      |            contextRef="D-2007-PPE-Buildings">This is information relating to depreciation method for
      |        Buildings.</gaap:PropertyPlantAndEquipmentDepreciationMethod>
      |    <gaap:PropertyPlantAndEquipmentEstimatedUsefulLife
      |            contextRef="D-2007-PPE-Buildings">This is information relating to the estimated useful life
      |        for Buildings.</gaap:PropertyPlantAndEquipmentEstimatedUsefulLife>
      |
      |    <gaap:PropertyPlantAndEquipmentMeasurementBasis
      |            contextRef="D-2007-PPE-FurnitureFixtures">This is information about the measurement basis for
      |        Furniture and Fixtures.
      |    </gaap:PropertyPlantAndEquipmentMeasurementBasis>
      |    <gaap:PropertyPlantAndEquipmentDepreciationMethod
      |            contextRef="D-2007-PPE-FurnitureFixtures">This is information relating to depreciation method for
      |        Furniture and Fixtures.
      |    </gaap:PropertyPlantAndEquipmentDepreciationMethod>
      |    <gaap:PropertyPlantAndEquipmentEstimatedUsefulLife
      |            contextRef="D-2007-PPE-FurnitureFixtures">This is information relating to the estimated useful life
      |        for Furniture and Fixtures.
      |    </gaap:PropertyPlantAndEquipmentEstimatedUsefulLife>
      |
      |    <gaap:PropertyPlantAndEquipmentMeasurementBasis
      |            contextRef="D-2007-PPE-Other">This is information about the measurement basis for Other.
      |    </gaap:PropertyPlantAndEquipmentMeasurementBasis>
      |    <gaap:PropertyPlantAndEquipmentDepreciationMethod
      |            contextRef="D-2007-PPE-Other">This is information relating to depreciation method for
      |        Other.</gaap:PropertyPlantAndEquipmentDepreciationMethod>
      |    <gaap:PropertyPlantAndEquipmentEstimatedUsefulLife
      |            contextRef="D-2007-PPE-Other">This is information relating to the estimated useful life
      |        for Other.</gaap:PropertyPlantAndEquipmentEstimatedUsefulLife>
      |
      |
      |    <gaap:PropertyPlantAndEquipmentDisclosures
      |            contextRef="D-2007">Etiam ipsum orci, gravida nec, feugiat ut, malesuada quis,
      |        mauris. Etiam porttitor. Ut venenatis, velit a accumsan interdum, odio
      |        metus mollis mauris, non pharetra augue arcu eu felis. Ut eget felis.
      |        Mauris leo nulla, sodales et, pharetra quis, fermentum nec, diam.
      |        Maecenas eget metus. Donec ultricies risus non dui. Ut ipsum mauris,
      |        pellentesque ut, tristique eget, fringilla id, ante. Vestibulum ante
      |        ipsum primis in faucibus orci luctus et ultrices posuere cubilia
      |        Curae; Duis erat. Curabitur non magna. Nullam wisi lectus, rutrum sit
      |        amet, fermentum et, venenatis hendrerit, magna. Suspendisse convallis
      |        urna elementum elit. </gaap:PropertyPlantAndEquipmentDisclosures>
      |    <gaap:PropertyPlantAndEquipmentUnderFinanceTypeLeases
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">10000
      |    </gaap:PropertyPlantAndEquipmentUnderFinanceTypeLeases>
      |    <gaap:PropertyPlantAndEquipmentNetProforma
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">10000
      |    </gaap:PropertyPlantAndEquipmentNetProforma>
      |
      |    <gaap:PropertyPlantAndEquipmentUnderFinanceTypeLeases
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">10000
      |    </gaap:PropertyPlantAndEquipmentUnderFinanceTypeLeases>
      |    <gaap:PropertyPlantAndEquipmentNetProforma
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">10000
      |    </gaap:PropertyPlantAndEquipmentNetProforma>
      |
      |
      |
      |    <!-- Property, Plant, and Equipment: Schedule, Movement Analysis -->
      |
      |    <gaap:LandAdditions contextRef="D-2007" unitRef="U-Monetary"
      |                        decimals="INF">1000</gaap:LandAdditions>
      |    <gaap:LandDisposals contextRef="D-2007" unitRef="U-Monetary"
      |                        decimals="INF">1000</gaap:LandDisposals>
      |    <gaap:LandTranslationDifference
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LandTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInLand
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInLand>
      |    <gaap:LandPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LandPeriodIncreaseDecrease>
      |
      |    <gaap:BuildingsNetAdditions contextRef="D-2007"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:BuildingsNetAdditions>
      |    <gaap:BuildingsNetDisposals contextRef="D-2007"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:BuildingsNetDisposals>
      |    <gaap:BuildingsNetTranslationDifference
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:BuildingsNetTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInBuildingsNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInBuildingsNet>
      |    <gaap:BuildingsNetPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:BuildingsNetPeriodIncreaseDecrease>
      |
      |    <gaap:FurnitureAndFixturesNetAdditions
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:FurnitureAndFixturesNetAdditions>
      |    <gaap:FurnitureAndFixturesNetDisposals
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:FurnitureAndFixturesNetDisposals>
      |    <gaap:FurnitureAndFixturesNetTranslationDifference
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:FurnitureAndFixturesNetTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInFurnitureAndFixturesNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInFurnitureAndFixturesNet>
      |    <gaap:FurnitureAndFixturesNetPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:FurnitureAndFixturesNetPeriodIncreaseDecrease>
      |
      |    <gaap:OtherPropertyPlantAndEquipmentNetAdditions
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherPropertyPlantAndEquipmentNetAdditions>
      |    <gaap:OtherPropertyPlantAndEquipmentNetDisposals
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherPropertyPlantAndEquipmentNetDisposals>
      |    <gaap:OtherPropertyPlantAndEquipmentNetTranslationDifference
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherPropertyPlantAndEquipmentNetTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInOtherPropertyPlantAndEquipmentNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInOtherPropertyPlantAndEquipmentNet>
      |    <gaap:OtherPropertyPlantAndEquipmentNetPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherPropertyPlantAndEquipmentNetPeriodIncreaseDecrease>
      |
      |    <gaap:PropertyPlantAndEquipmentNetAdditions
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:PropertyPlantAndEquipmentNetAdditions>
      |    <gaap:PropertyPlantAndEquipmentNetDisposals
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:PropertyPlantAndEquipmentNetDisposals>
      |    <gaap:PropertyPlantAndEquipmentNetTranslationDifference
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:PropertyPlantAndEquipmentNetTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInPropertyPlantAndEquipmentNet
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInPropertyPlantAndEquipmentNet>
      |    <gaap:PropertyPlantAndEquipmentNetPeriodIncreaseDecrease
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:PropertyPlantAndEquipmentNetPeriodIncreaseDecrease>
      |
      |
      |    <gaap:LandAdditions contextRef="D-2006" unitRef="U-Monetary"
      |                        decimals="INF">1000</gaap:LandAdditions>
      |    <gaap:LandDisposals contextRef="D-2006" unitRef="U-Monetary"
      |                        decimals="INF">1000</gaap:LandDisposals>
      |    <gaap:LandTranslationDifference
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LandTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInLand
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInLand>
      |    <gaap:LandPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LandPeriodIncreaseDecrease>
      |
      |    <gaap:BuildingsNetAdditions contextRef="D-2006"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:BuildingsNetAdditions>
      |    <gaap:BuildingsNetDisposals contextRef="D-2006"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:BuildingsNetDisposals>
      |    <gaap:BuildingsNetTranslationDifference
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:BuildingsNetTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInBuildingsNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInBuildingsNet>
      |    <gaap:BuildingsNetPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:BuildingsNetPeriodIncreaseDecrease>
      |
      |    <gaap:FurnitureAndFixturesNetAdditions
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:FurnitureAndFixturesNetAdditions>
      |    <gaap:FurnitureAndFixturesNetDisposals
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:FurnitureAndFixturesNetDisposals>
      |    <gaap:FurnitureAndFixturesNetTranslationDifference
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:FurnitureAndFixturesNetTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInFurnitureAndFixturesNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInFurnitureAndFixturesNet>
      |    <gaap:FurnitureAndFixturesNetPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:FurnitureAndFixturesNetPeriodIncreaseDecrease>
      |
      |    <gaap:OtherPropertyPlantAndEquipmentNetAdditions
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherPropertyPlantAndEquipmentNetAdditions>
      |    <gaap:OtherPropertyPlantAndEquipmentNetDisposals
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:OtherPropertyPlantAndEquipmentNetDisposals>
      |    <gaap:OtherPropertyPlantAndEquipmentNetTranslationDifference
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherPropertyPlantAndEquipmentNetTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInOtherPropertyPlantAndEquipmentNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInOtherPropertyPlantAndEquipmentNet>
      |    <gaap:OtherPropertyPlantAndEquipmentNetPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherPropertyPlantAndEquipmentNetPeriodIncreaseDecrease>
      |
      |    <gaap:PropertyPlantAndEquipmentNetAdditions
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:PropertyPlantAndEquipmentNetAdditions>
      |    <gaap:PropertyPlantAndEquipmentNetDisposals
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:PropertyPlantAndEquipmentNetDisposals>
      |    <gaap:PropertyPlantAndEquipmentNetTranslationDifference
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:PropertyPlantAndEquipmentNetTranslationDifference>
      |    <gaap:OtherIncreaseDecreaseInPropertyPlantAndEquipmentNet
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:OtherIncreaseDecreaseInPropertyPlantAndEquipmentNet>
      |    <gaap:PropertyPlantAndEquipmentNetPeriodIncreaseDecrease
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:PropertyPlantAndEquipmentNetPeriodIncreaseDecrease>
      |
      |
      |    <!-- Property, Plant, and Equipment: Schedule of Leasholod Land and Buildings -->
      |
      |    <gaap:LeaseholdLandAndBuildingLandArea
      |            contextRef="I-2007-LI-ALL" unitRef="U-Pure" decimals="INF">101000
      |    </gaap:LeaseholdLandAndBuildingLandArea>
      |    <gaap:LeaseholdLandAndBuildingValueAtCost
      |            contextRef="I-2007-LI-ALL" unitRef="U-Monetary" decimals="INF">55000
      |    </gaap:LeaseholdLandAndBuildingValueAtCost>
      |    <gaap:LeaseholdLandAndBuildingValueAtCost
      |            contextRef="I-2006-LI-ALL" unitRef="U-Monetary" decimals="INF">44000
      |    </gaap:LeaseholdLandAndBuildingValueAtCost>
      |
      |    <gaap:LeaseholdLandAndBuildingLocation
      |            contextRef="D-2007-LIA">Tacoma, Washington</gaap:LeaseholdLandAndBuildingLocation>
      |    <gaap:LeaseholdLandAndBuildingDescriptionOfFacility
      |            contextRef="D-2007-LIA">Warehouse
      |    </gaap:LeaseholdLandAndBuildingDescriptionOfFacility>
      |    <gaap:LeaseholdLandAndBuildingTenure
      |            contextRef="D-2007-LIA">Fifteen year lease</gaap:LeaseholdLandAndBuildingTenure>
      |    <gaap:LeaseholdLandAndBuildingTenureStartDate
      |            contextRef="D-2007-LIA">2000-01-01</gaap:LeaseholdLandAndBuildingTenureStartDate>
      |    <gaap:LeaseholdLandAndBuildingLandArea
      |            contextRef="I-2007-LIA" unitRef="U-Pure" decimals="INF">1000
      |    </gaap:LeaseholdLandAndBuildingLandArea>
      |    <gaap:LeaseholdLandAndBuildingValueAtCost
      |            contextRef="I-2007-LIA" unitRef="U-Monetary" decimals="INF">5000
      |    </gaap:LeaseholdLandAndBuildingValueAtCost>
      |    <gaap:LeaseholdLandAndBuildingValueAtCost
      |            contextRef="I-2006-LIA" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:LeaseholdLandAndBuildingValueAtCost>
      |
      |""".stripMargin

  private val xmlStringPart7 =
    """    <gaap:LeaseholdLandAndBuildingLocation
      |            contextRef="D-2007-LIB">Seattle, Washington</gaap:LeaseholdLandAndBuildingLocation>
      |    <gaap:LeaseholdLandAndBuildingDescriptionOfFacility
      |            contextRef="D-2007-LIB">Warehouse
      |    </gaap:LeaseholdLandAndBuildingDescriptionOfFacility>
      |    <gaap:LeaseholdLandAndBuildingTenure
      |            contextRef="D-2007-LIB">Twenty year lease</gaap:LeaseholdLandAndBuildingTenure>
      |    <gaap:LeaseholdLandAndBuildingTenureStartDate
      |            contextRef="D-2007-LIB">2000-01-01</gaap:LeaseholdLandAndBuildingTenureStartDate>
      |    <gaap:LeaseholdLandAndBuildingLandArea
      |            contextRef="I-2007-LIB" unitRef="U-Pure" decimals="INF">100000
      |    </gaap:LeaseholdLandAndBuildingLandArea>
      |    <gaap:LeaseholdLandAndBuildingValueAtCost
      |            contextRef="I-2007-LIB" unitRef="U-Monetary" decimals="INF">50000
      |    </gaap:LeaseholdLandAndBuildingValueAtCost>
      |    <gaap:LeaseholdLandAndBuildingValueAtCost
      |            contextRef="I-2006-LIB" unitRef="U-Monetary" decimals="INF">40000
      |    </gaap:LeaseholdLandAndBuildingValueAtCost>
      |
      |
      |
      |    <!-- Other Assets -->
      |    <gaap:OtherAssetsPolicy contextRef="D-2007">Etiam ipsum
      |        orci, gravida nec, feugiat ut, malesuada quis, mauris. Etiam
      |        porttitor. Ut venenatis, velit a accumsan interdum, odio metus mollis
      |        mauris, non pharetra augue arcu eu felis. Ut eget felis. Mauris leo
      |        nulla, sodales et, pharetra quis, fermentum nec, diam. Maecenas eget
      |        metus. Donec ultricies risus non dui. Ut ipsum mauris, pellentesque
      |        ut, tristique eget, fringilla id, ante. Vestibulum ante ipsum primis
      |        in faucibus orci luctus et ultrices posuere cubilia Curae; Duis erat.
      |        Curabitur non magna. Nullam wisi lectus, rutrum sit amet, fermentum
      |        et, venenatis hendrerit, magna. Suspendisse convallis urna elementum
      |        elit.</gaap:OtherAssetsPolicy>
      |    <gaap:OtherAssetsDisclosures contextRef="D-2007">Etiam
      |        ipsum orci, gravida nec, feugiat ut, malesuada quis, mauris. Etiam
      |        porttitor. Ut venenatis, velit a accumsan interdum, odio metus mollis
      |        mauris, non pharetra augue arcu eu felis. Ut eget felis. Mauris leo
      |        nulla, sodales et, pharetra quis, fermentum nec, diam. Maecenas eget
      |        metus. Donec ultricies risus non dui. Ut ipsum mauris, pellentesque
      |        ut, tristique eget, fringilla id, ante. Vestibulum ante ipsum primis
      |        in faucibus orci luctus et ultrices posuere cubilia Curae; Duis erat.
      |        Curabitur non magna. Nullam wisi lectus, rutrum sit amet, fermentum
      |        et, venenatis hendrerit, magna. Suspendisse convallis urna elementum
      |        elit.</gaap:OtherAssetsDisclosures>
      |    <gaap:OtherAssetsCurrentDisclosures
      |            contextRef="D-2007">Etiam ipsum orci, gravida nec, feugiat ut, malesuada quis,
      |        mauris. Etiam porttitor. Ut venenatis, velit a accumsan interdum, odio
      |        metus mollis mauris, non pharetra augue arcu eu felis. Ut eget felis.
      |        Mauris leo nulla, sodales et, pharetra quis, fermentum nec, diam.
      |        Maecenas eget metus. Donec ultricies risus non dui. Ut ipsum mauris,
      |        pellentesque ut, tristique eget, fringilla id, ante. Vestibulum ante
      |        ipsum primis in faucibus orci luctus et ultrices posuere cubilia
      |        Curae; Duis erat. Curabitur non magna. Nullam wisi lectus, rutrum sit
      |        amet, fermentum et, venenatis hendrerit, magna. Suspendisse convallis
      |        urna elementum elit.</gaap:OtherAssetsCurrentDisclosures>
      |    <gaap:OtherAssetsNoncurrentDisclosures
      |            contextRef="D-2007">Etiam ipsum orci, gravida nec, feugiat ut, malesuada quis,
      |        mauris. Etiam porttitor. Ut venenatis, velit a accumsan interdum, odio
      |        metus mollis mauris, non pharetra augue arcu eu felis. Ut eget felis.
      |        Mauris leo nulla, sodales et, pharetra quis, fermentum nec, diam.
      |        Maecenas eget metus. Donec ultricies risus non dui. Ut ipsum mauris,
      |        pellentesque ut, tristique eget, fringilla id, ante. Vestibulum ante
      |        ipsum primis in faucibus orci luctus et ultrices posuere cubilia
      |        Curae; Duis erat. Curabitur non magna. Nullam wisi lectus, rutrum sit
      |        amet, fermentum et, venenatis hendrerit, magna. Suspendisse convallis
      |        urna elementum elit.</gaap:OtherAssetsNoncurrentDisclosures>
      |
      |
      |    <!-- Payables and Accruals -->
      |
      |    <gaap:PayablesAndAccrualsDisclosures
      |            contextRef="D-2007">Etiam ipsum orci, gravida nec, feugiat ut, malesuada quis,
      |        mauris. Etiam porttitor. Ut venenatis, velit a accumsan interdum, odio
      |        metus mollis mauris, non pharetra augue arcu eu felis. Ut eget felis.
      |        Mauris leo nulla, sodales et, pharetra quis, fermentum nec, diam.
      |        Maecenas eget metus. Donec ultricies risus non dui.
      |    </gaap:PayablesAndAccrualsDisclosures>
      |    <gaap:AccountsPayableDisclosures
      |            contextRef="D-2007">Etiam porttitor. Ut venenatis, velit a accumsan interdum,
      |        odio metus mollis mauris, non pharetra augue arcu eu felis. Ut eget
      |        felis. Mauris leo nulla, sodales et, pharetra quis, fermentum nec,
      |        diam. Maecenas eget metus. Donec ultricies risus non dui. Etiam ipsum
      |        orci, gravida nec, feugiat ut, malesuada quis, mauris. Etiam
      |        porttitor. Ut venenatis, velit a accumsan interdum, odio metus mollis
      |        mauris, non pharetra augue arcu eu felis. Ut eget felis. Mauris leo
      |        nulla, sodales et, pharetra quis, fermentum nec, diam. Maecenas eget
      |        metus. Donec ultricies risus non dui.Etiam ipsum orci, gravida nec,
      |        feugiat ut, malesuada quis, mauris.</gaap:AccountsPayableDisclosures>
      |    <gaap:AccruedInterestPayableDisclosures
      |            contextRef="D-2007">Etiam ipsum orci, gravida nec, feugiat ut, malesuada quis,
      |        mauris. Etiam porttitor. Ut venenatis, velit a accumsan interdum, odio
      |        metus mollis mauris, non pharetra augue arcu eu felis. Ut eget felis.
      |        Mauris leo nulla, sodales et, pharetra quis, fermentum nec, diam.
      |        Maecenas eget metus. Donec ultricies risus non dui.
      |    </gaap:AccruedInterestPayableDisclosures>
      |    <gaap:AccruedExpensesDisclosures
      |            contextRef="D-2007">Ut venenatis, velit a accumsan interdum, odio metus mollis
      |        mauris, non pharetra augue arcu eu felis. Ut eget felis. Mauris leo
      |        nulla, sodales et, pharetra quis, fermentum nec, diam. Maecenas eget
      |        metus. Donec ultricies risus non dui.
      |    </gaap:AccruedExpensesDisclosures>
      |    <gaap:OtherPayablesAndAccrualsDisclosures
      |            contextRef="D-2007">Etiam porttitor. Ut venenatis, velit a accumsan interdum,
      |        odio metus mollis mauris, non pharetra augue arcu eu felis. Ut eget
      |        felis. Mauris leo nulla, sodales et, pharetra quis, fermentum nec,
      |        diam. Maecenas eget metus. Donec ultricies risus non dui.
      |    </gaap:OtherPayablesAndAccrualsDisclosures>
      |
      |
      |    <!-- Income Taxes -->
      |
      |    <gaap:IncomeTaxExpenseBenefitCurrentForeign
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">200
      |    </gaap:IncomeTaxExpenseBenefitCurrentForeign>
      |    <gaap:IncomeTaxExpenseBenefitCurrentDomestic
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">50
      |    </gaap:IncomeTaxExpenseBenefitCurrentDomestic>
      |    <gaap:IncomeTaxExpenseBenefitCurrent
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:IncomeTaxExpenseBenefitCurrent>
      |    <gaap:IncomeTaxExpenseBenefitDeferredForeign
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">200
      |    </gaap:IncomeTaxExpenseBenefitDeferredForeign>
      |    <gaap:IncomeTaxExpenseBenefitDeferredDomestic
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">50
      |    </gaap:IncomeTaxExpenseBenefitDeferredDomestic>
      |    <gaap:IncomeTaxExpenseBenefitDeferred
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:IncomeTaxExpenseBenefitDeferred>
      |
      |    <gaap:IncomeTaxExpenseBenefitCurrentForeign
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:IncomeTaxExpenseBenefitCurrentForeign>
      |    <gaap:IncomeTaxExpenseBenefitCurrentDomestic
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:IncomeTaxExpenseBenefitCurrentDomestic>
      |    <gaap:IncomeTaxExpenseBenefitCurrent
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:IncomeTaxExpenseBenefitCurrent>
      |    <gaap:IncomeTaxExpenseBenefitDeferredForeign
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:IncomeTaxExpenseBenefitDeferredForeign>
      |    <gaap:IncomeTaxExpenseBenefitDeferredDomestic
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:IncomeTaxExpenseBenefitDeferredDomestic>
      |    <gaap:IncomeTaxExpenseBenefitDeferred
      |            contextRef="D-2006" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:IncomeTaxExpenseBenefitDeferred>
      |
      |
      |
      |    <!-- Debt -->
      |
      |    <gaap:LongTermDebtSecured contextRef="I-2007"
      |                              unitRef="U-Monetary" decimals="INF">750</gaap:LongTermDebtSecured>
      |    <gaap:LongTermDebtUnsecured contextRef="I-2007"
      |                                unitRef="U-Monetary" decimals="INF">250</gaap:LongTermDebtUnsecured>
      |    <gaap:LongTermDebtSubordinated
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:LongTermDebtSubordinated>
      |    <gaap:OtherLongTermDebt contextRef="I-2007"
      |                            unitRef="U-Monetary" decimals="INF">250</gaap:OtherLongTermDebt>
      |
      |    <gaap:LongTermDebtSecured contextRef="I-2006"
      |                              unitRef="U-Monetary" decimals="INF">750</gaap:LongTermDebtSecured>
      |    <gaap:LongTermDebtUnsecured contextRef="I-2006"
      |                                unitRef="U-Monetary" decimals="INF">250</gaap:LongTermDebtUnsecured>
      |    <gaap:LongTermDebtSubordinated
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">250
      |    </gaap:LongTermDebtSubordinated>
      |    <gaap:OtherLongTermDebt contextRef="I-2006"
      |                            unitRef="U-Monetary" decimals="INF">250</gaap:OtherLongTermDebt>
      |
      |    <gaap:DebtDisclosures contextRef="D-2007"> Etiam porttitor.
      |        Ut venenatis, velit a accumsan interdum, odio metus mollis mauris, non
      |        pharetra augue arcu eu felis. Ut eget felis. Mauris leo nulla, sodales
      |        et, pharetra quis, fermentum nec, diam. Maecenas eget metus. Donec
      |        ultricies risus non dui.</gaap:DebtDisclosures>
      |    <gaap:LongTermDebtDisclosures contextRef="D-2007">Aliquam
      |        viverra risus vitae ante. Sed rhoncus mi in wisi. Nullam nibh dui,
      |        molestie vitae, imperdiet non, ornare at, elit. Aenean nec justo.
      |        Vestibulum ante ipsum primis in faucibus orci luctus et ultrices
      |        posuere cubilia Curae; Duis sodales.</gaap:LongTermDebtDisclosures>
      |
      |    <!-- Maturities -->
      |    <gaap:LongTermDebtMaturingInTwoAndThreeYears
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LongTermDebtMaturingInTwoAndThreeYears>
      |    <gaap:LongTermDebtMaturingInTwoYears
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LongTermDebtMaturingInTwoYears>
      |    <gaap:LongTermDebtMaturingInThreeYears
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LongTermDebtMaturingInThreeYears>
      |    <gaap:LongTermDebtMaturingInFourAndFiveYears
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LongTermDebtMaturingInFourAndFiveYears>
      |    <gaap:LongTermDebtMaturingInFourYears
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LongTermDebtMaturingInFourYears>
      |    <gaap:LongTermDebtMaturingInFiveYears
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:LongTermDebtMaturingInFiveYears>
      |    <gaap:LongTermDebtMaturingThereafter
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:LongTermDebtMaturingThereafter>
      |
      |    <!-- This fact value is added to make a calculation not show an inconsistency. -->
      |    <gaap:LongTermDebtMaturingThereafter
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:LongTermDebtMaturingThereafter>
      |
      |
      |    <!-- Debt Instruments -->
      |    <gaap:DebtInstrumentDescription
      |            contextRef="D-2007-DIA">Debt Instrument A in metus augue, euismod nec, luctus eu,
      |        egestas sed</gaap:DebtInstrumentDescription>
      |    <gaap:DebtInstrumentAmount contextRef="I-2007-DIA"
      |                               unitRef="U-Monetary" decimals="INF">500</gaap:DebtInstrumentAmount>
      |    <gaap:DebtInstrumentAmount contextRef="I-2006-DIA"
      |                               unitRef="U-Monetary" decimals="INF">500</gaap:DebtInstrumentAmount>
      |
      |    <gaap:DebtInstrumentDescription
      |            contextRef="D-2007-DIB">Debt Instrument B luctus eu, egestas sed
      |    </gaap:DebtInstrumentDescription>
      |    <gaap:DebtInstrumentAmount contextRef="I-2007-DIB"
      |                               unitRef="U-Monetary" decimals="INF">500</gaap:DebtInstrumentAmount>
      |    <gaap:DebtInstrumentAmount contextRef="I-2006-DIB"
      |                               unitRef="U-Monetary" decimals="INF">500</gaap:DebtInstrumentAmount>
      |
      |    <gaap:DebtInstrumentDescription
      |            contextRef="D-2007-DIC">Debt Instrument C velit lobortis dictum. In metus augue,
      |        euismod nec, luctus eu</gaap:DebtInstrumentDescription>
      |    <gaap:DebtInstrumentAmount contextRef="I-2007-DIC"
      |                               unitRef="U-Monetary" decimals="INF">500</gaap:DebtInstrumentAmount>
      |    <gaap:DebtInstrumentAmount contextRef="I-2006-DIC"
      |                               unitRef="U-Monetary" decimals="INF">500</gaap:DebtInstrumentAmount>
      |
      |    <gaap:DebtInstrumentAmount contextRef="I-2007-DI-ALL"
      |                               unitRef="U-Monetary" decimals="INF">1500</gaap:DebtInstrumentAmount>
      |    <gaap:DebtInstrumentAmount contextRef="I-2006-DI-ALL"
      |                               unitRef="U-Monetary" decimals="INF">1500</gaap:DebtInstrumentAmount>
      |
      |
      |    <!-- Other Liabilities -->
      |    <gaap:OtherLiabilitiesDisclosures
      |            contextRef="D-2007">Blah blah blah.</gaap:OtherLiabilitiesDisclosures>
      |    <gaap:OtherLiabilitiesCurrentDisclosures
      |            contextRef="D-2007">Blah blah blah.</gaap:OtherLiabilitiesCurrentDisclosures>
      |    <gaap:OtherLiabilitiesNoncurrentDisclosures
      |            contextRef="D-2007">Blah blah blah.
      |    </gaap:OtherLiabilitiesNoncurrentDisclosures>
      |
      |
      |    <!-- Equity -->
      |
      |    <gaap:OwnersEquity contextRef="I-2007" unitRef="U-Monetary"
      |                       decimals="INF">0</gaap:OwnersEquity>
      |    <gaap:PartnersEquity contextRef="I-2007" unitRef="U-Monetary"
      |                         decimals="INF">0</gaap:PartnersEquity>
      |    <gaap:MembersEquity contextRef="I-2007" unitRef="U-Monetary"
      |                        decimals="INF">0</gaap:MembersEquity>
      |
      |    <gaap:OwnersEquity contextRef="I-2006" unitRef="U-Monetary"
      |                       decimals="INF">0</gaap:OwnersEquity>
      |    <gaap:PartnersEquity contextRef="I-2006" unitRef="U-Monetary"
      |                         decimals="INF">0</gaap:PartnersEquity>
      |    <gaap:MembersEquity contextRef="I-2006" unitRef="U-Monetary"
      |                        decimals="INF">0</gaap:MembersEquity>
      |
      |    <gaap:CommonStockDisclosures contextRef="D-2007">Blah
      |        blah blah.</gaap:CommonStockDisclosures>
      |    <gaap:PreferredStockDisclosures
      |            contextRef="D-2007">Blah blah blah.</gaap:PreferredStockDisclosures>
      |    <gaap:AdditionalPaidInCapitalDisclosures
      |            contextRef="D-2007">Blah blah blah.</gaap:AdditionalPaidInCapitalDisclosures>
      |    <gaap:RetainedEarningsAccumulatedLossesDisclosures
      |            contextRef="D-2007">Blah blah blah.
      |    </gaap:RetainedEarningsAccumulatedLossesDisclosures>
      |
      |
      |
      |    <!-- Business Segments -->
      |
      |    <!-- Current period -->
      |    <gaap:ExternalSales contextRef="D-2007-BS1" unitRef="U-Monetary"
      |                        decimals="INF">2000</gaap:ExternalSales>
      |    <gaap:ExternalSales contextRef="D-2007-BS2" unitRef="U-Monetary"
      |                        decimals="INF">2000</gaap:ExternalSales>
      |    <gaap:ExternalSales contextRef="D-2007-CON" unitRef="U-Monetary"
      |                        decimals="INF">4000</gaap:ExternalSales>
      |    <gaap:ExternalSales contextRef="D-2007-ALL" unitRef="U-Monetary"
      |                        decimals="INF">4000</gaap:ExternalSales>
      |    <gaap:ExternalSales contextRef="D-2007-E" unitRef="U-Monetary"
      |                        decimals="INF">0</gaap:ExternalSales>
      |    <gaap:ExternalSales contextRef="D-2007" unitRef="U-Monetary"
      |                        decimals="INF">4000</gaap:ExternalSales>
      |
      |    <gaap:IntersegmentalSales contextRef="D-2007-BS1"
      |                              unitRef="U-Monetary" decimals="INF">1000</gaap:IntersegmentalSales>
      |    <gaap:IntersegmentalSales contextRef="D-2007-BS2"
      |                              unitRef="U-Monetary" decimals="INF">1000</gaap:IntersegmentalSales>
      |    <gaap:IntersegmentalSales contextRef="D-2007-CON"
      |                              unitRef="U-Monetary" decimals="INF">2000</gaap:IntersegmentalSales>
      |    <gaap:IntersegmentalSales contextRef="D-2007-ALL"
      |                              unitRef="U-Monetary" decimals="INF">2000</gaap:IntersegmentalSales>
      |    <gaap:IntersegmentalSales contextRef="D-2007-E"
      |                              unitRef="U-Monetary" decimals="INF">-2000</gaap:IntersegmentalSales>
      |    <gaap:IntersegmentalSales contextRef="D-2007"
      |                              unitRef="U-Monetary" decimals="INF">0</gaap:IntersegmentalSales>
      |
      |    <gaap:RevenuesNet contextRef="D-2007-BS1" unitRef="U-Monetary"
      |                      decimals="INF">3000</gaap:RevenuesNet>
      |    <gaap:RevenuesNet contextRef="D-2007-BS2" unitRef="U-Monetary"
      |                      decimals="INF">3000</gaap:RevenuesNet>
      |    <gaap:RevenuesNet contextRef="D-2007-CON" unitRef="U-Monetary"
      |                      decimals="INF">6000</gaap:RevenuesNet>
      |    <gaap:RevenuesNet contextRef="D-2007-ALL" unitRef="U-Monetary"
      |                      decimals="INF">6000</gaap:RevenuesNet>
      |    <gaap:RevenuesNet contextRef="D-2007-E" unitRef="U-Monetary"
      |                      decimals="INF">-2000</gaap:RevenuesNet>
      |    <!-- NOTE that the total of this is consolidated revenu in the context D-2007,
      |		needs default dimensions declared. -->
      |
      |    <gaap:SegmentResult contextRef="D-2007-BS1" unitRef="U-Monetary"
      |                        decimals="INF">2000</gaap:SegmentResult>
      |    <gaap:SegmentResult contextRef="D-2007-BS2" unitRef="U-Monetary"
      |                        decimals="INF">2000</gaap:SegmentResult>
      |    <gaap:SegmentResult contextRef="D-2007-CON" unitRef="U-Monetary"
      |                        decimals="INF">4000</gaap:SegmentResult>
      |    <gaap:SegmentResult contextRef="D-2007-ALL" unitRef="U-Monetary"
      |                        decimals="INF">4000</gaap:SegmentResult>
      |    <gaap:SegmentResult contextRef="D-2007-E" unitRef="U-Monetary"
      |                        decimals="INF">-1000</gaap:SegmentResult>
      |    <gaap:SegmentResult contextRef="D-2007" unitRef="U-Monetary"
      |                        decimals="INF">3000</gaap:SegmentResult>
      |
      |
      |    <gaap:UnallocatedCorporateExpenses
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:UnallocatedCorporateExpenses>
      |
      |    <gaap:IncomeLossFromAffiliates
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:IncomeLossFromAffiliates>
      |    <gaap:IncomeLossFromInvestments
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:IncomeLossFromInvestments>
      |
      |    <gaap:CapitalAdditions contextRef="D-2007-BS1"
      |                           unitRef="U-Monetary" decimals="INF">750</gaap:CapitalAdditions>
      |    <gaap:CapitalAdditions contextRef="D-2007-BS2"
      |                           unitRef="U-Monetary" decimals="INF">750</gaap:CapitalAdditions>
      |    <gaap:CapitalAdditions contextRef="D-2007-CON"
      |                           unitRef="U-Monetary" decimals="INF">1500</gaap:CapitalAdditions>
      |    <gaap:CapitalAdditions contextRef="D-2007-ALL"
      |                           unitRef="U-Monetary" decimals="INF">1500</gaap:CapitalAdditions>
      |    <gaap:CapitalAdditions contextRef="D-2007-E"
      |                           unitRef="U-Monetary" decimals="INF">-500</gaap:CapitalAdditions>
      |    <gaap:CapitalAdditions contextRef="D-2007"
      |                           unitRef="U-Monetary" decimals="INF">1000</gaap:CapitalAdditions>
      |
      |    <gaap:Depreciation contextRef="D-2007-BS2" unitRef="U-Monetary"
      |                       decimals="INF">250</gaap:Depreciation>
      |    <gaap:Depreciation contextRef="D-2007-BS1" unitRef="U-Monetary"
      |                       decimals="INF">250</gaap:Depreciation>
      |    <gaap:Depreciation contextRef="D-2007-CON" unitRef="U-Monetary"
      |                       decimals="INF">500</gaap:Depreciation>
      |    <gaap:Depreciation contextRef="D-2007-ALL" unitRef="U-Monetary"
      |                       decimals="INF">500</gaap:Depreciation>
      |    <gaap:Depreciation contextRef="D-2007-E" unitRef="U-Monetary"
      |                       decimals="INF">0</gaap:Depreciation>
      |    <gaap:Depreciation contextRef="D-2007" unitRef="U-Monetary"
      |                       decimals="INF">500</gaap:Depreciation>
      |
      |    <gaap:Amortization contextRef="D-2007-BS1" unitRef="U-Monetary"
      |                       decimals="INF">250</gaap:Amortization>
      |    <gaap:Amortization contextRef="D-2007-BS2" unitRef="U-Monetary"
      |                       decimals="INF">250</gaap:Amortization>
      |    <gaap:Amortization contextRef="D-2007-CON" unitRef="U-Monetary"
      |                       decimals="INF">500</gaap:Amortization>
      |    <gaap:Amortization contextRef="D-2007-ALL" unitRef="U-Monetary"
      |                       decimals="INF">500</gaap:Amortization>
      |    <gaap:Amortization contextRef="D-2007-E" unitRef="U-Monetary"
      |                       decimals="INF">0</gaap:Amortization>
      |    <gaap:Amortization contextRef="D-2007" unitRef="U-Monetary"
      |                       decimals="INF">500</gaap:Amortization>
      |
      |    <gaap:DepreciationAndAmortization
      |            contextRef="D-2007-BS1" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:DepreciationAndAmortization>
      |    <gaap:DepreciationAndAmortization
      |            contextRef="D-2007-BS2" unitRef="U-Monetary" decimals="INF">500
      |    </gaap:DepreciationAndAmortization>
      |    <gaap:DepreciationAndAmortization
      |            contextRef="D-2007-CON" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:DepreciationAndAmortization>
      |    <gaap:DepreciationAndAmortization
      |            contextRef="D-2007-ALL" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:DepreciationAndAmortization>
      |    <gaap:DepreciationAndAmortization
      |            contextRef="D-2007-E" unitRef="U-Monetary" decimals="INF">0
      |    </gaap:DepreciationAndAmortization>
      |
      |    <gaap:ImpairmentLossesReversalsRecognizedInIncome
      |            contextRef="D-2007-BS1" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:ImpairmentLossesReversalsRecognizedInIncome>
      |    <gaap:ImpairmentLossesReversalsRecognizedInIncome
      |            contextRef="D-2007-BS2" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:ImpairmentLossesReversalsRecognizedInIncome>
      |    <gaap:ImpairmentLossesReversalsRecognizedInIncome
      |            contextRef="D-2007-CON" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:ImpairmentLossesReversalsRecognizedInIncome>
      |    <gaap:ImpairmentLossesReversalsRecognizedInIncome
      |            contextRef="D-2007-ALL" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:ImpairmentLossesReversalsRecognizedInIncome>
      |    <gaap:ImpairmentLossesReversalsRecognizedInIncome
      |            contextRef="D-2007-E" unitRef="U-Monetary" decimals="INF">-3000
      |    </gaap:ImpairmentLossesReversalsRecognizedInIncome>
      |    <gaap:ImpairmentLossesReversalsRecognizedInIncome
      |            contextRef="D-2007" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:ImpairmentLossesReversalsRecognizedInIncome>
      |
      |    <gaap:AssetsSegmental contextRef="I-2007-BS1"
      |                          unitRef="U-Monetary" decimals="INF">5000</gaap:AssetsSegmental>
      |    <gaap:AssetsSegmental contextRef="I-2007-BS2"
      |                          unitRef="U-Monetary" decimals="INF">5000</gaap:AssetsSegmental>
      |    <gaap:AssetsSegmental contextRef="I-2007-CON"
      |                          unitRef="U-Monetary" decimals="INF">10000</gaap:AssetsSegmental>
      |    <gaap:AssetsSegmental contextRef="I-2007-ALL"
      |                          unitRef="U-Monetary" decimals="INF">10000</gaap:AssetsSegmental>
      |    <gaap:AssetsSegmental contextRef="I-2007-E"
      |                          unitRef="U-Monetary" decimals="INF">-2000</gaap:AssetsSegmental>
      |    <gaap:AssetsSegmental contextRef="I-2007" unitRef="U-Monetary"
      |                          decimals="INF">8000</gaap:AssetsSegmental>
      |
      |    <gaap:AssetsUnallocatedCorporate
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">4000
      |    </gaap:AssetsUnallocatedCorporate>
      |
      |    <gaap:LiabilitiesSegmental contextRef="I-2007-BS1"
      |                               unitRef="U-Monetary" decimals="INF">3000</gaap:LiabilitiesSegmental>
      |    <gaap:LiabilitiesSegmental contextRef="I-2007-BS2"
      |                               unitRef="U-Monetary" decimals="INF">3000</gaap:LiabilitiesSegmental>
      |    <gaap:LiabilitiesSegmental contextRef="I-2007-CON"
      |                               unitRef="U-Monetary" decimals="INF">6000</gaap:LiabilitiesSegmental>
      |    <gaap:LiabilitiesSegmental contextRef="I-2007-ALL"
      |                               unitRef="U-Monetary" decimals="INF">6000</gaap:LiabilitiesSegmental>
      |    <gaap:LiabilitiesSegmental contextRef="I-2007-E"
      |                               unitRef="U-Monetary" decimals="INF">-2000</gaap:LiabilitiesSegmental>
      |    <gaap:LiabilitiesSegmental contextRef="I-2007"
      |                               unitRef="U-Monetary" decimals="INF">4000</gaap:LiabilitiesSegmental>
      |
      |    <gaap:LiabilitiesUnallocatedCorporate
      |            contextRef="I-2007" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:LiabilitiesUnallocatedCorporate>
      |
      |    <gaap:AverageNumberEmployees contextRef="D-2007-BS1"
      |                                 unitRef="U-Pure" decimals="INF">200</gaap:AverageNumberEmployees>
      |    <gaap:AverageNumberEmployees contextRef="D-2007-BS2"
      |                                 unitRef="U-Pure" decimals="INF">100</gaap:AverageNumberEmployees>
      |
      |
      |    <!-- Prior period -->
      |    <gaap:AssetsSegmental contextRef="I-2006" unitRef="U-Monetary"
      |                          decimals="INF">8000</gaap:AssetsSegmental>
      |    <gaap:AssetsUnallocatedCorporate
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:AssetsUnallocatedCorporate>
      |
      |
      |    <gaap:LiabilitiesSegmental contextRef="I-2006"
      |                               unitRef="U-Monetary" decimals="INF">4000</gaap:LiabilitiesSegmental>
      |    <gaap:LiabilitiesUnallocatedCorporate
      |            contextRef="I-2006" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:LiabilitiesUnallocatedCorporate>
      |
      |    <gaap:Depreciation contextRef="D-2006" unitRef="U-Monetary"
      |                       decimals="INF">0</gaap:Depreciation>
      |    <gaap:Amortization contextRef="D-2006" unitRef="U-Monetary"
      |                       decimals="INF">0</gaap:Amortization>
      |
      |
      |
      |
      |    <!-- Share Ownership Plans -->
      |
      |    <gaap:ShareOwnershipPlanDescriptionGeneralTermsAndConditions
      |            contextRef="D-2007-SOP1">These are the description, general conditions, and terms of
      |        share ownership plan 1
      |    </gaap:ShareOwnershipPlanDescriptionGeneralTermsAndConditions>
      |    <gaap:ShareOwnershipPlanShareOptionsOutstanding
      |            contextRef="I-2006-SOP1" unitRef="U-Shares" decimals="INF">0
      |    </gaap:ShareOwnershipPlanShareOptionsOutstanding>
      |    <gaap:ShareOwnershipPlanShareOptionsOutstanding
      |            contextRef="I-2007-SOP1" unitRef="U-Shares" decimals="INF">1000
      |    </gaap:ShareOwnershipPlanShareOptionsOutstanding>
      |    <gaap:ShareOwnershipPlanShareOptionsGranted
      |            contextRef="D-2007-SOP1" unitRef="U-Shares" decimals="INF">4000
      |    </gaap:ShareOwnershipPlanShareOptionsGranted>
      |    <gaap:ShareOwnershipPlanShareOptionsForfeited
      |            contextRef="D-2007-SOP1" unitRef="U-Shares" decimals="INF">1000
      |    </gaap:ShareOwnershipPlanShareOptionsForfeited>
      |    <gaap:ShareOwnershipPlanShareOptionsExercised
      |            contextRef="D-2007-SOP1" unitRef="U-Shares" decimals="INF">1000
      |    </gaap:ShareOwnershipPlanShareOptionsExercised>
      |    <gaap:ShareOwnershipPlanShareOptionsExpired
      |            contextRef="D-2007-SOP1" unitRef="U-Shares" decimals="INF">1000
      |    </gaap:ShareOwnershipPlanShareOptionsExpired>
      |    <gaap:ShareOwnershipPlanShareOptionsOutstandingPeriodIncreaseDecrease
      |            contextRef="D-2007-SOP1" unitRef="U-Shares" decimals="INF">1000
      |    </gaap:ShareOwnershipPlanShareOptionsOutstandingPeriodIncreaseDecrease>
      |
      |
      |
      |    <!-- Subsequent Events -->
      |
      |    <gaap:SubsequentEventDescription
      |            contextRef="D-2007-SE-1">Description of subsequent event number 1 which relates to
      |        the loss of an uncollectable receivable.
      |    </gaap:SubsequentEventDescription>
      |    <gaap:SubsequentEventDate contextRef="D-2007-SE-1">2008-01-15
      |    </gaap:SubsequentEventDate>
      |
      |    <gaap:SubsequentEventDescription
      |            contextRef="D-2007-SE-2">Description of subsequent event number 2 which relates to
      |        the purchase of a business.</gaap:SubsequentEventDescription>
      |    <gaap:SubsequentEventDate contextRef="D-2007-SE-2">2008-01-20
      |    </gaap:SubsequentEventDate>
      |
      |
      |    <!-- Other Overall Financial Reporting Presentation and Display -->
      |
      |    <gaap:NatureBusiness contextRef="D-2007">The consolidated
      |        financial statements include the accounts of ABC Company, Inc. and its
      |        subsidiaries, all wholly owned. All significant intercompany balances
      |        and transactions have been eliminated in consolidation.
      |    </gaap:NatureBusiness>
      |    <gaap:ReclassificationOfFinancialStatementItems
      |            contextRef="D-2007">Some prior period classifications have been changed to
      |        conform to current period classifications.
      |    </gaap:ReclassificationOfFinancialStatementItems>
      |    <gaap:AverageNumberEmployees contextRef="D-2007"
      |                                 unitRef="U-Pure" decimals="INF">300</gaap:AverageNumberEmployees>
      |    <gaap:AverageNumberEmployees contextRef="D-2006"
      |                                 unitRef="U-Pure" decimals="INF">250</gaap:AverageNumberEmployees>
      |
      |
      |
      |    <!-- Related Parties -->
      |
      |    <gaap:RelatedPartyTypeOfRelationship
      |            contextRef="D-2007-RP1">Parent</gaap:RelatedPartyTypeOfRelationship>
      |    <gaap:RelatedPartyNatureOfRelationship
      |            contextRef="D-2007-RP1">This is other descriptive information about the
      |        relationship.</gaap:RelatedPartyNatureOfRelationship>
      |
      |    <gaap:RelatedPartyTypeOfRelationship
      |            contextRef="D-2007-RP2">JointVenture</gaap:RelatedPartyTypeOfRelationship>
      |    <gaap:RelatedPartyNatureOfRelationship
      |            contextRef="D-2007-RP2">This is other descriptive information about the
      |        relationship.</gaap:RelatedPartyNatureOfRelationship>
      |
      |    <gaap:RelatedPartyTransactionDescription
      |            contextRef="D-2007-RP1T1">Transaction 1 description
      |    </gaap:RelatedPartyTransactionDescription>
      |    <gaap:RelatedPartyTransactionPricingPolicy
      |            contextRef="D-2007-RP1T1">Cost</gaap:RelatedPartyTransactionPricingPolicy>
      |    <gaap:RelatedPartyTransactionAmount
      |            contextRef="D-2007-RP1T1" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:RelatedPartyTransactionAmount>
      |
      |    <gaap:RelatedPartyTransactionDescription
      |            contextRef="D-2007-RP1T2">Transaction 2 description
      |    </gaap:RelatedPartyTransactionDescription>
      |    <gaap:RelatedPartyTransactionPricingPolicy
      |            contextRef="D-2007-RP1T2">Cost</gaap:RelatedPartyTransactionPricingPolicy>
      |    <gaap:RelatedPartyTransactionAmount
      |            contextRef="D-2007-RP1T2" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:RelatedPartyTransactionAmount>
      |
      |
      |    <gaap:RelatedPartyTransactionDescription
      |            contextRef="D-2007-RP2T1">Transaction 1 description
      |    </gaap:RelatedPartyTransactionDescription>
      |    <gaap:RelatedPartyTransactionPricingPolicy
      |            contextRef="D-2007-RP2T1">Cost</gaap:RelatedPartyTransactionPricingPolicy>
      |    <gaap:RelatedPartyTransactionAmount
      |            contextRef="D-2007-RP2T1" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:RelatedPartyTransactionAmount>
      |
      |    <gaap:RelatedPartyTransactionDescription
      |            contextRef="D-2007-RP2T2">Transaction 2 description
      |    </gaap:RelatedPartyTransactionDescription>
      |    <gaap:RelatedPartyTransactionPricingPolicy
      |            contextRef="D-2007-RP2T2">Cost</gaap:RelatedPartyTransactionPricingPolicy>
      |    <gaap:RelatedPartyTransactionAmount
      |            contextRef="D-2007-RP2T2" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:RelatedPartyTransactionAmount>
      |
      |
      |""".stripMargin

  private val xmlStringPart8 =
    """    <!-- Director's Compensation -->
      |
      |    <gaap:DirectorSalary contextRef="D-2007-DIR1"
      |                         unitRef="U-Monetary" decimals="INF">1000</gaap:DirectorSalary>
      |    <gaap:DirectorBonuses contextRef="D-2007-DIR1"
      |                          unitRef="U-Monetary" decimals="INF">1000</gaap:DirectorBonuses>
      |    <gaap:DirectorFees contextRef="D-2007-DIR1" unitRef="U-Monetary"
      |                       decimals="INF">1000</gaap:DirectorFees>
      |    <gaap:DirectorSalaryBonusesAndFees
      |            contextRef="D-2007-DIR1" unitRef="U-Monetary" decimals="INF">3000
      |    </gaap:DirectorSalaryBonusesAndFees>
      |    <gaap:DirectorOptionsGrantedAtFairValue
      |            contextRef="D-2007-DIR1" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:DirectorOptionsGrantedAtFairValue>
      |
      |    <gaap:DirectorSalary contextRef="D-2007-DIR2"
      |                         unitRef="U-Monetary" decimals="INF">1000</gaap:DirectorSalary>
      |    <gaap:DirectorBonuses contextRef="D-2007-DIR2"
      |                          unitRef="U-Monetary" decimals="INF">1000</gaap:DirectorBonuses>
      |    <gaap:DirectorFees contextRef="D-2007-DIR2" unitRef="U-Monetary"
      |                       decimals="INF">1000</gaap:DirectorFees>
      |    <gaap:DirectorSalaryBonusesAndFees
      |            contextRef="D-2007-DIR2" unitRef="U-Monetary" decimals="INF">3000
      |    </gaap:DirectorSalaryBonusesAndFees>
      |    <gaap:DirectorOptionsGrantedAtFairValue
      |            contextRef="D-2007-DIR2" unitRef="U-Monetary" decimals="INF">1000
      |    </gaap:DirectorOptionsGrantedAtFairValue>
      |
      |    <gaap:DirectorSalary contextRef="D-2007-DIR-ALL"
      |                         unitRef="U-Monetary" decimals="INF">2000</gaap:DirectorSalary>
      |    <gaap:DirectorBonuses contextRef="D-2007-DIR-ALL"
      |                          unitRef="U-Monetary" decimals="INF">2000</gaap:DirectorBonuses>
      |    <gaap:DirectorFees contextRef="D-2007-DIR-ALL"
      |                       unitRef="U-Monetary" decimals="INF">2000</gaap:DirectorFees>
      |    <gaap:DirectorSalaryBonusesAndFees
      |            contextRef="D-2007-DIR-ALL" unitRef="U-Monetary" decimals="INF">6000
      |    </gaap:DirectorSalaryBonusesAndFees>
      |    <gaap:DirectorOptionsGrantedAtFairValue
      |            contextRef="D-2007-DIR-ALL" unitRef="U-Monetary" decimals="INF">2000
      |    </gaap:DirectorOptionsGrantedAtFairValue>
      |
      |
      |    <!-- Reconciliation of Cash -->
      |    <gaap:ReconcilingItems contextRef="I-2007"
      |                           unitRef="U-Monetary" decimals="INF">-1000</gaap:ReconcilingItems>
      |    <gaap:ReconcilingItems contextRef="I-2006"
      |                           unitRef="U-Monetary" decimals="INF">0</gaap:ReconcilingItems>
      |
      |    <gaap:ReconcilingItemDescription
      |            contextRef="D-2007-CREC-A">Reconciling Item A</gaap:ReconcilingItemDescription>
      |    <gaap:ReconcilingItemAmount contextRef="I-2007-CREC-A"
      |                                unitRef="U-Monetary" decimals="INF">-500</gaap:ReconcilingItemAmount>
      |    <gaap:ReconcilingItemAmount contextRef="I-2006-CREC-A"
      |                                unitRef="U-Monetary" decimals="INF">500</gaap:ReconcilingItemAmount>
      |
      |    <gaap:ReconcilingItemDescription
      |            contextRef="D-2007-CREC-B">Reconciling Item B</gaap:ReconcilingItemDescription>
      |    <gaap:ReconcilingItemAmount contextRef="I-2007-CREC-B"
      |                                unitRef="U-Monetary" decimals="INF">-500</gaap:ReconcilingItemAmount>
      |    <gaap:ReconcilingItemAmount contextRef="I-2006-CREC-B"
      |                                unitRef="U-Monetary" decimals="INF">-500</gaap:ReconcilingItemAmount>
      |
      |    <!-- THIS IS A DUPLICATE, FIX WHEN DEFAULT DIMENSIONS ARE FIXED -->
      |    <gaap:ReconcilingItemAmount contextRef="I-2007-CREC-ALL"
      |                                unitRef="U-Monetary" decimals="INF">-1000</gaap:ReconcilingItemAmount>
      |    <gaap:ReconcilingItemAmount contextRef="I-2006-CREC-ALL"
      |                                unitRef="U-Monetary" decimals="INF">0</gaap:ReconcilingItemAmount>
      |
      |
      |    <!-- Portfolio of Investments -->
      |    <gaap:InvestmentShares contextRef="I-2007-INV1"
      |                           unitRef="U-Shares" decimals="INF">10000</gaap:InvestmentShares>
      |    <gaap:InvestmentValueAtCost contextRef="I-2007-INV1"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:InvestmentValueAtCost>
      |
      |    <gaap:InvestmentShares contextRef="I-2007-INV2"
      |                           unitRef="U-Shares" decimals="INF">10000</gaap:InvestmentShares>
      |    <gaap:InvestmentValueAtCost contextRef="I-2007-INV2"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:InvestmentValueAtCost>
      |
      |
      |    <gaap:InvestmentShares contextRef="I-2006-INV3"
      |                           unitRef="U-Shares" decimals="INF">10000</gaap:InvestmentShares>
      |    <gaap:InvestmentValueAtCost contextRef="I-2006-INV3"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:InvestmentValueAtCost>
      |
      |    <gaap:InvestmentShares contextRef="I-2006-INV4"
      |                           unitRef="U-Shares" decimals="INF">10000</gaap:InvestmentShares>
      |    <gaap:InvestmentValueAtCost contextRef="I-2006-INV4"
      |                                unitRef="U-Monetary" decimals="INF">1000</gaap:InvestmentValueAtCost>
      |
      |    <!-- THIS IS A DUPLICATE, FIX WHEN DEFAULT DIMENSIONS ARE FIXED -->
      |    <gaap:InvestmentShares contextRef="I-2007-INV-ALL"
      |                           unitRef="U-Shares" decimals="INF">20000</gaap:InvestmentShares>
      |    <gaap:InvestmentValueAtCost contextRef="I-2007-INV-ALL"
      |                                unitRef="U-Monetary" decimals="INF">2000</gaap:InvestmentValueAtCost>
      |
      |    <gaap:InvestmentShares contextRef="I-2006-INV-ALL"
      |                           unitRef="U-Shares" decimals="INF">20000</gaap:InvestmentShares>
      |    <gaap:InvestmentValueAtCost contextRef="I-2006-INV-ALL"
      |                                unitRef="U-Monetary" decimals="INF">2000</gaap:InvestmentValueAtCost>
      |
      |
      |
      |    <!-- Revenue Analysis -->
      |
      |    <gaap:RevenuesNet contextRef="D-2007-ABC1" unitRef="U-Monetary"
      |                      decimals="INF">2000</gaap:RevenuesNet>
      |    <gaap:RevenuesNet contextRef="D-2007-ABC2" unitRef="U-Monetary"
      |                      decimals="INF">1000</gaap:RevenuesNet>
      |    <gaap:RevenuesNet contextRef="D-2007-ABC3" unitRef="U-Monetary"
      |                      decimals="INF">1000</gaap:RevenuesNet>
      |
      |    <!-- THIS IS A DUPLICATE, FIX WHEN DEFAULT DIMENSIONS ARE FIXED -->
      |    <gaap:RevenuesNet contextRef="D-2007-ABC-ALL" unitRef="U-Monetary"
      |                      decimals="INF">4000</gaap:RevenuesNet>
      |
      |
      |    <!-- Management Discussion and Analysis -->
      |    <gaap:ManagementDiscussionAndAnalysisTextBlock
      |            contextRef="D-2007"><![CDATA[
      |The following is an example/sample of the target use case for narratives.  The information was taken
      |from various existing samples and put together to minimize the effort required to put this example
      |together.  It may seem nonsensical, but it definitely shows the Actual use case.  The Actual use case
      |has the following characteristics:
      |
      |     1) It contains a mixture of paragraphs of text and tables of information,
      |     2) The information MUST be viewed in a particular order to make sense,
      |     3) There is typically a large volume of information, such as the "Management Discussion and
      |        Analysis",
      |     4) The information can be unique to a company, no standard taxonomy concepts exist.
      |     5) The information contains a mixture of "data" and "labels".
      |
      |This example is intended to mimic the characteristics of the use case described above.  It would
      |likely be quite easy to find an example from a financial report which shows this use case in a
      |form which is more comfortable to domain users (i.e., accountants).  If the domain users would like
      |this example updated to be more understandable to an accountant, we can put that together
      |for them.  But, this example is to show the characteristics of the use case with minimum
      |effort, not to be 100% correct from a financial reporting perspective.
      |
      |
      |RECEIVABLES
      |
      |Trade receivables are stated at their nominal value as reduced by appropriate allowances for estimated
      |irrecoverable amounts.
      |
      |Trade receivables include receivables from major customers.
      |
      |Trade receivables include related party receivables.
      |
      |
      |DIRECTOR COMPENSATION
      |
      |The following is a listing of director compensation:
      |
      |                                                                  Fair Value of
      |Name of director             Salary         Bonus Director fees Options Granted
      |===================== ============= ============= ============= ===============
      |John James                        0             0        60,000               0
      |Buck Rogers                 879,639     1,213,486             0         569,000
      |Clark Kent                        0             0        24,200               0
      |Lois Lane                         0             0        57,000               0
      |
      |
      |ACCOUNTING POLICIES
      |
      |The financial statements have been prepared on the historical cost basis, except for the revaluation of land
      |and buildings and certain financial instruments. The principal accounting policies adopted are set out below.
      |
      |Inventories
      |
      |Inventories are stated at the lower of cost and net realizable value. Cost comprises direct materials and,
      |where applicable, direct labor costs and those overheads that have been incurred in bringing the
      |inventories to their present location and condition. Cost is calculated using the weighted average method.
      |
      |Net realizable value represents the estimated selling price less all estimated costs to completion and costs to
      |be incurred in marketing, selling and distribution.
      |
      |Inventories are stated at the lower of cost and net realizable value. Included in inventory are: (a) raw
      |materials, (b) supplies, (c) finished goods.
      |
      |
      |LONG TERM DEBT
      |
      |The following is a summary of Long-term Debt outstanding as of December 31, 2004 and 2003:
      |
      |                                                                             2004      2003
      |                                                                        ========= =========
      |Note payable to a bank, principal payments of $11,300 due September
      |15 and October 15, 2004, 2005, and 2006 with an additional principal
      |payment of $880 paid when the note is due on October 15, 2006,
      |interest at prime plus 2% is payable monthly, secured by equipment       $ 23,480  $ 46,080
      |
      |Note payable to a bank, principal payments of $3,400 due monthly from
      |August through January, interest at prime plus 2% payable monthly, due
      |October 5, 2009, secured by a vehicle                                      85,000         0
      |
      |Note payable to a related party, payable in semi-annual principal
      |installments of $10,000 plus interest at 10%, unsecured                         0    45,000
      |
      |Capital lease payable in monthly installments of $1,200 including
      |interest at 13.2%, due February 2007, secured by a vehicle                 33,301    43,782
      |                                                                        --------- ---------
      |          Long Term Debt, Total                                           141,781   134,862
      |
      |Current Portion of Long Term-Debt                                          23,000    22,000
      |                                                                        --------- ---------
      |
      |          Non Current Portion of Long Term Debt                         $ 118,781 $ 112,862
      |                                                                        ========= =========
      |
      |
      |FINANCIAL INSTRUMENTS
      |
      |Financial assets and financial liabilities are recognized on the Group's balance sheet when the Group has
      |become a party to the contractual provisions of the instrument.
      |]]>
      |    </gaap:ManagementDiscussionAndAnalysisTextBlock>
      |
      |
      |    <!-- General Comments -->
      |    <link:footnoteLink xlink:title='General Comment'
      |                       xlink:type="extended" xlink:role="http://www.xbrl.org/2003/role/link">
      |        <link:loc xlink:type="locator" xlink:href="#Item-01"
      |                  xlink:label="Item-01" />
      |        <link:loc xlink:type="locator" xlink:href="#Item-02"
      |                  xlink:label="Item-02" />
      |        <link:footnoteArc xlink:type="arc"
      |                          xlink:arcrole="http://www.xbrl.org/2003/arcrole/fact-footnote"
      |                          xlink:from="Item-01" xlink:to="Footnote-01" order="1" />
      |        <link:footnoteArc xlink:type="arc"
      |                          xlink:arcrole="http://www.xbrl.org/2003/arcrole/fact-footnote"
      |                          xlink:from="Item-02" xlink:to="Footnote-01" order="2" />
      |        <link:footnote xlink:type="resource"
      |                       xlink:role="http://www.xbrl.org/2003/role/footnote" xlink:label="Footnote-01"
      |                       xml:lang="en">GENERAL: This is a footnote making general comments.
      |        </link:footnote>
      |    </link:footnoteLink>
      |
      |    <!-- Restatement -->
      |    <link:footnoteLink xlink:title='Restatement Comment'
      |                       xlink:type="extended" xlink:role="http://www.xbrl.org/2003/role/link">
      |        <link:loc xlink:type="locator" xlink:href="#Restated-01"
      |                  xlink:label="Restated-01" />
      |        <link:loc xlink:type="locator" xlink:href="#Restated-02"
      |                  xlink:label="Restated-02" />
      |        <link:loc xlink:type="locator" xlink:href="#Restated-03"
      |                  xlink:label="Restated-03" />
      |        <link:footnoteArc xlink:type="arc"
      |                          xlink:arcrole="http://www.xbrl.org/2003/arcrole/fact-footnote"
      |                          xlink:from="Restated-01" xlink:to="Footnote-02" order="1" />
      |        <link:footnoteArc xlink:type="arc"
      |                          xlink:arcrole="http://www.xbrl.org/2003/arcrole/fact-footnote"
      |                          xlink:from="Restated-02" xlink:to="Footnote-02" order="2" />
      |        <link:footnoteArc xlink:type="arc"
      |                          xlink:arcrole="http://www.xbrl.org/2003/arcrole/fact-footnote"
      |                          xlink:from="Restated-03" xlink:to="Footnote-02" order="3" />
      |        <link:footnote xlink:type="resource"
      |                       xlink:role="http://www.xbrl.org/2003/role/footnote" xlink:label="Footnote-02"
      |                       xml:lang="en">RESTATEMENT: This is a footnote relating to a restatement.
      |        </link:footnote>
      |    </link:footnoteLink>
      |
      |</xbrl>""".stripMargin

  val xmlString = Seq(xmlStringPart1, xmlStringPart2, xmlStringPart3, xmlStringPart4,
    xmlStringPart5, xmlStringPart6, xmlStringPart7, xmlStringPart8).mkString
}
