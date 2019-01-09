=======
Yaidom2
=======

Introduction
============

This project is a playground for developing the next generation of the `yaidom`_ library. Yaidom is essentially a
Scala XML query API. It is used heavily at `EBPI`_ for `XBRL`_ processing, although yaidom is an XML API without any
XBRL knowledge.

There are several reasons to work on a 2nd generation of yaidom. Hence this project. Some of these reasons are:

* Yaidom is starting to show its age. Having learned which parts carry their weight and which parts don't, yaidom should become smaller, cleaner but also more consistent.
* Yaidom is being used for in-memory querying/processing of many thousands of XML documents simultaneously, so performance and memory footprint are quite important. It therefore helps leverage the improved collections API in (upcoming) Scala 2.13.0.
* With the Saxon 9.9 library, the state of the art of XML processing in Java has improved dramatically, so good interop with Saxon 9.9 may be desirable.

.. _`yaidom`: https://github.com/dvreeze/yaidom
.. _`EBPI`: https://ebpi.nl/
.. _`XBRL`: https://www.xbrl.org/

Yaidom philosophy
=================

The yaidom project has the following philosophy:

* Yaidom is most of all an **XML "DOM" query API**, offering a **Scala Collections API** query experience
* XML **namespaces** are first-class citizens in yaidom and its query API, far more so than isolated namespace prefixes
* The query API is simple and **element-node-centric**, instead of being based on unrestricted XPath axes that can traverse nodes of any kind (like Saxon 9.9)
* This query API reflects the fact that we can regard XML at **different abstraction levels**:

  * The most abstract level is that of "James Clark" elements, using "expanded names" for element names and attribute names
  * At a somewhat lower level, XML elements also know about qualified names and in-scope namespaces
  * At a more practical (and lower) level, XML elements also know about "context", such as parent element, document URI, base URI, etc.
  * This layering is reflected in the query API by having the lower abstraction levels extending the query API of the previous level
  * Due to these layers that are intentionally less rich than the XPath Data Model, the query API can be said to support only the most common XPath axes, and only for element nodes
  * Moreover, none of these layers has any knowledge about XML Schema

* Yaidom offers **multiple element implementations** offering the yaidom query API (at one of the above-mentioned levels):

  * Some element implementations are native to yaidom (at least one for each of the 3 query API abstraction levels)
  * Some element implementations are wrappers around "DOM" trees of other XML libraries, such as Saxon, with its fast and memory-efficient tiny trees
  * Other element implementations offering the same uniform XML query API are relatively easy to add
  * Conversions between different element implementations are also made easy (using the layered query API)

* The yaidom query API itself is **not streaming**, but yaidom still makes it possible to "stream" very large XML documents using its StAX support
* Yaidom offers support for so-called **yaidom dialects**:

  * A yaidom dialect is an XML dialect (typically described by an XML Schema) expressed as a type-safe query API that knows about different kinds of elements
  * This is facilitated by a specific query API trait for querying elements of a certain type
  * A yaidom dialect can be backed by different element implementations offering the same query API, without the need for any custom coding

* Yaidom can target multiple platforms and platform versions:

  * On the Java platform, multiple Java versions and Scala versions
  * The same on JavaScript platforms, making yaidom usable in the browser using ScalaJS

* Yaidom does not reinvent the wheel, but leverages JAXP, etc.
* The library offers some support to make it relatively easy to use in Java projects, in combination with XPath, etc.
* Yaidom tries to be **conceptually small** and **precise**, like in a simple mathematical theory
* Performance, usability and **production-readiness** are important

The "James Clark" XML "DOM" trees are described in `XML Namespaces`_. The Saxon library is documented at `Saxon`_.

.. _`XML Namespaces`: http://www.jclark.com/xml/xmlns.htm
.. _`Saxon`: https://www.saxonica.com/documentation

Yaidom2 philosophy
==================

TODO

Usage
=====

Yaidom2 versions can be found in the Maven central repository. Assuming version 0.1.0, yaidom2 can be added as dependency
as follows (in an SBT or Maven build):

**SBT**::

    libraryDependencies += "eu.cdevreeze.yaidom2" %%% "yaidom2" % "0.1.0"

**Maven2**::

    <dependency>
      <groupId>eu.cdevreeze.yaidom2</groupId>
      <artifactId>yaidom2_2.13.0-M5</artifactId>
      <version>0.1.0</version>
    </dependency>

Note that yaidom2 itself has a few dependencies, which will be transitive dependencies in projects that use yaidom2.

Yaidom2 requires Java version 1.8 or later.
