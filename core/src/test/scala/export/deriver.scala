/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import export._, classwithderivation._

package object tcderiver {
  val exports = DerivedTc.exports
}

package tcderiver {
  import shapeless._

  trait DerivedTc[T] extends Tc[T]

  @exports
  object DerivedTc extends DerivedTc0 {
    implicit def hnil: DerivedTc[HNil] =
      new DerivedTc[HNil] {
        def describe: String = "HNil"
      }

    implicit def hcons[H, T <: HList](implicit hd: Lazy[Tc[H]], tl: Lazy[DerivedTc[T]]): DerivedTc[H :: T] =
      new DerivedTc[H :: T] {
        def describe: String = s"${hd.value.describe} :: ${tl.value.describe}"
      }

    implicit def cnil: DerivedTc[CNil] =
      new DerivedTc[CNil] {
        def describe: String = "CNil"
      }

    implicit def ccons[H, T <: Coproduct](implicit hd: Lazy[Tc[H]], tl: Lazy[DerivedTc[T]]): DerivedTc[H :+: T] =
      new DerivedTc[H :+: T] {
        def describe: String = s"${hd.value.describe} :+: ${tl.value.describe}"
      }
  }

  trait DerivedTc0 {
    implicit def gen[T, R](implicit gen: Generic.Aux[T, R], mtcr: Lazy[DerivedTc[R]]): DerivedTc[T] =
      new DerivedTc[T] {
        def describe: String = s"gen(${mtcr.value.describe})"
      }
  }
}

