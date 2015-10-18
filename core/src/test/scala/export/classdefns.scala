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

import adtdefns._

package classwithderivation {
  import export._

  trait Tc[T] {
    def describe: String = safeDescribe(Set.empty)
    def safeDescribe(seen: Set[Any]): String
  }

  object Tc extends TcDefault {
    def apply[T](implicit mtct: Tc[T]): Tc[T] = mtct

    implicit val intInst: Tc[Int] =
      new Tc[Int] {
        def safeDescribe(seen: Set[Any]) = "Tc[Int]"
      }

    implicit val fooInst: Tc[Foo] =
      new Tc[Foo] {
        def safeDescribe(seen: Set[Any]) = "Tc[Foo]"
      }
  }

  @imports[Tc]
  trait TcDefault {
    implicit def default[T] =
      new Tc[T] {
        def safeDescribe(seen: Set[Any]) = "Default Tc[T]"
      }
  }
}

package classwithsubclasses {
  import export._

  // Type class definition
  trait Tc[T] {
    def describe: String = safeDescribe(Set.empty)
    def safeDescribe(seen: Set[Any]): String
  }

  // Include module-local subclass instances
  object Tc extends TcInstances
  trait TcInstances extends TcDefault {
    def apply[T](implicit mtct: Tc[T]): Tc[T] = mtct

    implicit val intInst: Tc[Int] =
      new Tc[Int] {
        def safeDescribe(seen: Set[Any]) = "Tc[Int]"
      }
  }

  @imports[Tc]
  trait TcDefault {
    implicit def default[T]: Tc[T] =
      new Tc[T] {
        def safeDescribe(seen: Set[Any]) = "Default Tc[T]"
      }
  }

  // Subclass within the same module
  trait TcSub[T] extends Tc[T]

  @exports
  object TcSub extends TcSubInstances
  trait TcSubInstances extends TcSubDefault {
    implicit val fooInst: TcSub[Foo] =
      new TcSub[Foo] {
        def safeDescribe(seen: Set[Any]) = "TcSub[Foo]"
      }
  }

  @imports[TcSub]
  trait TcSubDefault {
    implicit def default[T]: TcSub[T] =
      new TcSub[T] {
        def safeDescribe(seen: Set[Any]) = "Default TcSub[T]"
      }
  }
}

package externalsubclass {
  import classwithsubclasses._, export._

  // Subclass in a different module
  trait TcExtSub[T] extends Tc[T]

  @exports
  object TcExtSub extends TcExtSubInstances
  trait TcExtSubInstances extends TcExtSubDefault {
    implicit def barInst: TcExtSub[Bar] =
      new TcExtSub[Bar] {
        def safeDescribe(seen: Set[Any]) = "TcExtSub[Bar]"
      }
  }

  @imports[TcExtSub]
  trait TcExtSubDefault {
    implicit def default[T]: TcExtSub[T] =
      new TcExtSub[T] {
        def safeDescribe(seen: Set[Any]) = "Default TcExtSub[T]"
      }
  }
}

package optional {
  import export._
  import classwithderivation._

  // Optional instances in a different module
  object optinstances {
    // Primitive instance
    implicit def boolInst: ExportOptional[Tc[Boolean]] =
      ExportOptional(
        new Tc[Boolean] {
          def safeDescribe(seen: Set[Any]) = "Tc[Boolean]"
        }
      )

    // Instance which could be derived
    implicit def barInst: ExportOptional[Tc[Bar]] =
      ExportOptional(
        new Tc[Bar] {
          def safeDescribe(seen: Set[Any]) = "Tc[Bar]"
        }
      )
  }
}

package higherkinded {
  import export._

  trait Tc1[F[_]] {
    def describe: String = safeDescribe(Set.empty)
    def safeDescribe(seen: Set[Any]): String
  }

  object Tc1 extends Tc1Default {
    def apply[F[_]](implicit mtcf: Tc1[F]): Tc1[F] = mtcf

    implicit val optionInst: Tc1[Option] =
      new Tc1[Option] {
        def safeDescribe(seen: Set[Any]) = "Tc1[Option]"
      }
  }

  @imports[Tc1]
  trait Tc1Default {
    implicit def default[F[_]] =
      new Tc1[F] {
        def safeDescribe(seen: Set[Any]) = "Default Tc1[F]"
      }
  }
}

package customprioritization {
  import export._

  object CustomPrioritization {
    implicit val priority =
      ExportPriority[
        ExportHighPriority,
        ExportOptional,
        ExportSubclass,
        ExportAlgebraic,
        ExportGeneric,
        ExportInstantiated,
        ExportDefault,
        ExportLowPriority
      ]
  }
}
