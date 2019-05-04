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

package eu.cdevreeze.yaidom2

/**
 * Support for "yaidom dialects", for example a "dialect" for XML Schema. Such dialects offer a yaidom query API. They
 * get an underlying element, which also offers a yaidom query API. Typically this yaidom query API (in both cases) is
 * the backing element query API.
 *
 * The types in this package make creation of "dialects" a lot easier. They take an underlying element of the "raw" type,
 * instead of requiring any type parameters. This makes it possible to decouple dialects entirely from the underlying
 * element implementations!
 *
 * Using a dialect abstract element type as super-type, it is easy to implement type-safe query methods for the specific
 * dialect.
 *
 * @author Chris de Vreeze
 */
package object dialect
