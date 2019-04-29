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

package eu.cdevreeze.yaidom2.queryapi.oo.elemstep

import eu.cdevreeze.yaidom2.queryapi.oo.ScopedElemApi
import eu.cdevreeze.yaidom2.queryapi.oo.ScopedElemStepFactoryApi

/**
 * ElemStep factory implementation for Scoped elements.
 *
 * @author Chris de Vreeze
 */
// scalastyle:off number.of.methods
abstract class ScopedElemStepFactory extends ClarkElemStepFactory with ScopedElemStepFactoryApi {

  type ElemType <: ScopedElemApi.Aux[ElemType]
}

object ScopedElemStepFactory {

  type Aux[E] = ScopedElemStepFactory { type ElemType = E }
}
