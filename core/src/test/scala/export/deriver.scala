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

package deriver

import export._

import shapeless._
import tca._, tcb._

@reexports[DerivedTcA]
object tcaderived

trait DerivedTcA[T] extends TcA[T]

@exports
object DerivedTcA extends DerivedTcA0 {
  implicit def hnil: DerivedTcA[HNil] =
    new DerivedTcA[HNil] {
      def safeDescribe(seen: Set[Any]) = "HNil"
    }

  implicit def hcons[H, T <: HList](implicit hd: Lazy[TcA[H]], tl: Lazy[DerivedTcA[T]]): DerivedTcA[H :: T] =
    new DerivedTcA[H :: T] {
      def safeDescribe(seen: Set[Any]) =
        if(seen(this)) "loop (hcons)" else {
          val seen1 = seen+this
          s"${hd.value.safeDescribe(seen1)} :: ${tl.value.safeDescribe(seen1)}"
        }
    }

  implicit def cnil: DerivedTcA[CNil] =
    new DerivedTcA[CNil] {
      def safeDescribe(seen: Set[Any]) = "CNil"
    }

  implicit def ccons[H, T <: Coproduct](implicit hd: Lazy[TcA[H]], tl: Lazy[DerivedTcA[T]]): DerivedTcA[H :+: T] =
    new DerivedTcA[H :+: T] {
      def safeDescribe(seen: Set[Any]) =
        if(seen(this)) "loop (ccons)" else {
          val seen1 = seen+this
          s"${hd.value.safeDescribe(seen1)} :+: ${tl.value.safeDescribe(seen1)}"
        }
    }
}

trait DerivedTcA0 {
  implicit def gen[T, R](implicit gen: Generic.Aux[T, R], mtcr: Lazy[DerivedTcA[R]]): DerivedTcA[T] =
    new DerivedTcA[T] {
      def safeDescribe(seen: Set[Any]) =
        if(seen(this)) "loop (gen)" else {
          val seen1 = seen+this
          s"gen(${mtcr.value.safeDescribe(seen1)})"
        }
    }
}

@reexports[DerivedTcB]
object tcbderived

trait DerivedTcB[T] extends TcB[T]

@exports
object DerivedTcB extends DerivedTcB0 {
  implicit def hnil: DerivedTcB[HNil] =
    new DerivedTcB[HNil] {
      def safeDescribe(seen: Set[Any]) = "HNil"
    }

  implicit def hcons[H, T <: HList](implicit hd: Lazy[TcB[H]], tl: Lazy[DerivedTcB[T]]): DerivedTcB[H :: T] =
    new DerivedTcB[H :: T] {
      def safeDescribe(seen: Set[Any]) =
        if(seen(this)) "loop (hcons)" else {
          val seen1 = seen+this
          s"${hd.value.safeDescribe(seen1)} :: ${tl.value.safeDescribe(seen1)}"
        }
    }

  implicit def cnil: DerivedTcB[CNil] =
    new DerivedTcB[CNil] {
      def safeDescribe(seen: Set[Any]) = "CNil"
    }

  implicit def ccons[H, T <: Coproduct](implicit hd: Lazy[TcB[H]], tl: Lazy[DerivedTcB[T]]): DerivedTcB[H :+: T] =
    new DerivedTcB[H :+: T] {
      def safeDescribe(seen: Set[Any]) =
        if(seen(this)) "loop (ccons)" else {
          val seen1 = seen+this
          s"${hd.value.safeDescribe(seen1)} :+: ${tl.value.safeDescribe(seen1)}"
        }
    }
}

trait DerivedTcB0 {
  implicit def gen[T, R](implicit gen: Generic.Aux[T, R], mtcr: Lazy[DerivedTcB[R]]): DerivedTcB[T] =
    new DerivedTcB[T] {
      def safeDescribe(seen: Set[Any]) =
        if(seen(this)) "loop (gen)" else {
          val seen1 = seen+this
          s"gen(${mtcr.value.safeDescribe(seen1)})"
        }
    }
}
