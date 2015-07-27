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

package tc

import defns.Foo
import derivation._

trait Tc[T] {
  def describe: String
}

object Tc extends TcDerivationHook {
  def apply[T](implicit mtct: Tc[T]): Tc[T] = mtct

  implicit val intInst: Tc[Int] =
    new Tc[Int] {
      def describe: String = "Tc[Int]"
    }

  implicit val fooInst: Tc[Foo] =
    new Tc[Foo] {
      def describe: String = "Tc[Foo]"
    }
}

trait TcDerivationHook extends TcDefault {
  implicit def derive[T](implicit deriver: Deriver0[Tc, T]): Tc[T] = deriver.derive
}

trait TcDefault {
  implicit def default[T] =
    new Tc[T] {
      def describe: String = "Default Tc[T]"
    }
}
