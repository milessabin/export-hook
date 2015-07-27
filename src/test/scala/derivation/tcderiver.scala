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

import derivation._, tc._, shapeless._

package object tcderiver {
  implicit def apply[T](implicit wtct: DerivedTc[T]): Deriver0[Tc, T] =
    new Deriver0[Tc, T] {
      def derive: Tc[T] = wtct
    }
}

package tcderiver {
  trait DerivedTc[T] extends Tc[T]

  object DerivedTc {
    implicit def hnil: DerivedTc[HNil] =
      new DerivedTc[HNil] {
        def describe: String = "HNil"
      }

    implicit def hcons[H, T <: HList](implicit hd: Tc[H], tl: Lazy[DerivedTc[T]]): DerivedTc[H :: T] =
      new DerivedTc[H :: T] {
        def describe: String = s"${hd.describe} :: ${tl.value.describe}"
      }

    implicit def cnil: DerivedTc[CNil] =
      new DerivedTc[CNil] {
        def describe: String = "CNil"
      }

    implicit def ccons[H, T <: Coproduct](implicit hd: Tc[H], tl: Lazy[DerivedTc[T]]): DerivedTc[H :+: T] =
      new DerivedTc[H :+: T] {
        def describe: String = s"${hd.describe} :+: ${tl.value.describe}"
      }

    implicit def gen[T, R](implicit gen: Generic.Aux[T, R], mtcr: DerivedTc[R]): DerivedTc[T] =
      new DerivedTc[T] {
        def describe: String = s"gen(${mtcr.describe})"
      }
  }
}

