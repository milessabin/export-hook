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

package tcuser

import org.scalatest.FunSuite

import adtdefns._

class DeriverTests extends FunSuite {
  import classwithderivation._, tcderiver.exports._

  test("Tc[Int]") {
    assert(Tc[Int].describe === "Tc[Int]")
  }

  test("Tc[Foo]") {
    assert(Tc[Foo].describe === "Tc[Foo]")
  }

  test("Tc[Bar]") {
    assert(Tc[Bar].describe === "gen(Tc[Int] :: Default Tc[T] :: HNil)")
  }

  test("Tc[Baz]") {
    assert(Tc[Baz].describe === "Tc[Baz]")
  }

  test("Tc[Double]") {
    assert(Tc[Double].describe === "Default Tc[T]")
  }

  test("Tc[Quux]") {
    assert(Tc[Quux].describe ===
      "gen(Tc[Foo] :: gen(Tc[Int] :: Default Tc[T] :: HNil) :: Tc[Baz] :: HNil)")
  }
}


class SubclassTests extends FunSuite {
  import classwithsubclasses._, externalsubclass._

  test("Tc[Int]") {
    assert(Tc[Int].describe === "Tc[Int]")
  }

  test("Tc[Foo]") {
    import TcSub.exports._
    assert(Tc[Foo].describe === "TcSub[Foo]")
  }

  test("Tc[Bar]") {
    import TcExtSub.exports._
    assert(Tc[Bar].describe === "TcExtSub[Bar]")
  }
}
