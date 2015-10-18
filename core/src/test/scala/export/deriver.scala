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

package object tcderiver {
  val exports = DerivedTc.exports
}

package tcderiver {
  import shapeless._
  import export._, classwithderivation._

  trait DerivedTc[T] extends Tc[T]

  @exports
  object DerivedTc extends DerivedTc0 {
    implicit def hnil: DerivedTc[HNil] =
      new DerivedTc[HNil] {
        def safeDescribe(seen: Set[Any]) = "HNil"
      }

    implicit def hcons[H, T <: HList](implicit hd: Lazy[Tc[H]], tl: Lazy[DerivedTc[T]]): DerivedTc[H :: T] =
      new DerivedTc[H :: T] {
        def safeDescribe(seen: Set[Any]) =
          if(seen(this)) "loop (hcons)" else {
            val seen1 = seen+this
            s"${hd.value.safeDescribe(seen1)} :: ${tl.value.safeDescribe(seen1)}"
          }
      }

    implicit def cnil: DerivedTc[CNil] =
      new DerivedTc[CNil] {
        def safeDescribe(seen: Set[Any]) = "CNil"
      }

    implicit def ccons[H, T <: Coproduct](implicit hd: Lazy[Tc[H]], tl: Lazy[DerivedTc[T]]): DerivedTc[H :+: T] =
      new DerivedTc[H :+: T] {
        def safeDescribe(seen: Set[Any]) =
          if(seen(this)) "loop (ccons)" else {
            val seen1 = seen+this
            s"${hd.value.safeDescribe(seen1)} :+: ${tl.value.safeDescribe(seen1)}"
          }
      }
  }

  trait DerivedTc0 {
    implicit def gen[T, R](implicit gen: Generic.Aux[T, R], mtcr: Lazy[DerivedTc[R]]): DerivedTc[T] =
      new DerivedTc[T] {
        def safeDescribe(seen: Set[Any]) =
          if(seen(this)) "loop (gen)" else {
            val seen1 = seen+this
            s"gen(${mtcr.value.safeDescribe(seen1)})"
          }
      }
  }
}
