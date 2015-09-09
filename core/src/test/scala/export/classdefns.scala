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
    def describe: String
  }

  object Tc extends TcDefault {
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

  @imports[Tc]
  trait TcDefault {
    implicit def default[T] =
      new Tc[T] {
        def describe: String = "Default Tc[T]"
      }
  }
}

package classwithsubclasses {
  import export._

  // Type class definition
  trait Tc[T] {
    def describe: String
  }

  // Include module-local subclass instances
  object Tc extends TcInstances
  trait TcInstances extends TcDefault {
    def apply[T](implicit mtct: Tc[T]): Tc[T] = mtct

    implicit val intInst: Tc[Int] =
      new Tc[Int] {
        def describe: String = "Tc[Int]"
      }
  }

  @imports[Tc]
  trait TcDefault {
    implicit def default[T]: Tc[T] =
      new Tc[T] {
        def describe: String = "Default Tc[T]"
      }
  }

  // Subclass within the same module
  trait TcSub[T] extends Tc[T]

  @exports
  object TcSub extends TcSubInstances
  trait TcSubInstances extends TcSubDefault {
    implicit val fooInst: TcSub[Foo] =
      new TcSub[Foo] {
        def describe: String = "TcSub[Foo]"
      }
  }

  @imports[TcSub]
  trait TcSubDefault {
    implicit def default[T]: TcSub[T] =
      new TcSub[T] {
        def describe: String = "Default TcSub[T]"
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
        def describe: String = "TcExtSub[Bar]"
      }
  }

  @imports[TcExtSub]
  trait TcExtSubDefault {
    implicit def default[T]: TcExtSub[T] =
      new TcExtSub[T] {
        def describe: String = "Default TcExtSub[T]"
      }
  }
}
