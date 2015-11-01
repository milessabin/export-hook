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

package tca {
  import export._

  trait TcA[T] {
    def describe: String = safeDescribe(Set.empty)
    def safeDescribe(seen: Set[Any]): String
  }

  object TcA extends TcADefault {
    def apply[T](implicit mtct: TcA[T]): TcA[T] = mtct

    implicit val intInst: TcA[Int] =
      new TcA[Int] {
        def safeDescribe(seen: Set[Any]) = "TcA[Int]"
      }

    implicit val fooInst: TcA[Foo] =
      new TcA[Foo] {
        def safeDescribe(seen: Set[Any]) = "TcA[Foo]"
      }
  }

  @imports[TcA]
  trait TcADefault {
    implicit def default[T] =
      new TcA[T] {
        def safeDescribe(seen: Set[Any]) = "Default TcA[T]"
      }
  }
}

package tcb {
  import export._

  // Type class definition
  trait TcB[T] {
    def describe: String = safeDescribe(Set.empty)
    def safeDescribe(seen: Set[Any]): String
  }

  // Include module-local subclass instances
  object TcB extends TcBInstances
  trait TcBInstances extends TcBDefault {
    def apply[T](implicit mtct: TcB[T]): TcB[T] = mtct

    implicit val intInst: TcB[Int] =
      new TcB[Int] {
        def safeDescribe(seen: Set[Any]) = "TcB[Int]"
      }
  }

  @imports[TcB]
  trait TcBDefault {
    implicit def default[T]: TcB[T] =
      new TcB[T] {
        def safeDescribe(seen: Set[Any]) = "Default TcB[T]"
      }
  }

  // Subclass within the same module
  trait TcBSub[T] extends TcB[T]

  @exports(Subclass)
  object TcBSub extends TcBSubInstances
  trait TcBSubInstances extends TcBSubDefault {
    implicit val fooInst: TcBSub[Foo] =
      new TcBSub[Foo] {
        def safeDescribe(seen: Set[Any]) = "TcBSub[Foo]"
      }
  }

  @imports[TcBSub]
  trait TcBSubDefault {
    implicit def default[T]: TcBSub[T] =
      new TcBSub[T] {
        def safeDescribe(seen: Set[Any]) = "Default TcBSub[T]"
      }
  }
}

package tcbext {
  import tcb._, export._

  // Subclass in a different module
  trait TcBExtSub[T] extends TcB[T]

  @exports(Subclass)
  object TcBExtSub extends TcBExtSubInstances
  trait TcBExtSubInstances extends TcBExtSubDefault {
    implicit def barInst: TcBExtSub[Bar] =
      new TcBExtSub[Bar] {
        def safeDescribe(seen: Set[Any]) = "TcBExtSub[Bar]"
      }
  }

  @imports[TcBExtSub]
  trait TcBExtSubDefault {
    implicit def default[T]: TcBExtSub[T] =
      new TcBExtSub[T] {
        def safeDescribe(seen: Set[Any]) = "Default TcBExtSub[T]"
      }
  }
}

package orphans {
  import export._
  import tca._

  // Orphan instances in a different module
  @reexports[OrphanTcA]
  object tcaorphans

  trait OrphanTcA[T] extends TcA[T]

  @exports(Orphan)
  object OrphanTcA {
    // Primitive instance
    implicit def boolInst: OrphanTcA[Boolean] =
      new OrphanTcA[Boolean] {
        def safeDescribe(seen: Set[Any]) = "TcA[Boolean]"
      }

    // Instance which could be derived
    implicit def barInst: OrphanTcA[Bar] =
      new OrphanTcA[Bar] {
        def safeDescribe(seen: Set[Any]) = "TcA[Bar]"
      }
  }
}

package tca1 {
  import export._

  trait TcA1[F[_]] {
    def describe: String = safeDescribe(Set.empty)
    def safeDescribe(seen: Set[Any]): String
  }

  object TcA1 extends TcA1Default {
    def apply[F[_]](implicit mtcf: TcA1[F]): TcA1[F] = mtcf

    implicit val optionInst: TcA1[Option] =
      new TcA1[Option] {
        def safeDescribe(seen: Set[Any]) = "TcA1[Option]"
      }
  }

  @imports[TcA1]
  trait TcA1Default {
    implicit def default[F[_]] =
      new TcA1[F] {
        def safeDescribe(seen: Set[Any]) = "Default TcA1[F]"
      }
  }
}

package customprioritization {
  import export._

  object CustomPrioritization {
    implicit val priority =
      ExportPriority[
        ExportHighPriority,
        ExportOrphan,
        ExportSubclass,
        ExportAlgebraic,
        ExportGeneric,
        ExportInstantiated,
        ExportDefault,
        ExportLowPriority
      ]
  }
}
