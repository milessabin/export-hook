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

import adtdefns._, autodefns._, tca._, tcb._

class ReexportTests extends FunSuite {
  test("Single reexport") {
    import single._

    assert(TcA[Int].describe === "TcA[Int]")
    assert(TcA[Foo].describe === "TcA[Foo]")
    assert(TcA[Boolean].describe === "TcA[Boolean]")
    assert(TcA[Bar].describe === "TcA[Bar]")
    assert(TcA[Quux].describe === "Default TcA[T]")

    assert(TcB[Int].describe === "TcB[Int]")
    assert(TcB[Foo].describe === "Default TcB[T]")
    assert(TcB[Boolean].describe === "Default TcB[T]")
    assert(TcB[Bar].describe === "Default TcB[T]")
    assert(TcB[Quux].describe === "Default TcB[T]")
  }

  test("Multi class reexport") {
    import twoclasses._

    assert(TcA[Int].describe === "TcA[Int]")
    assert(TcA[Foo].describe === "TcA[Foo]")
    assert(TcA[Boolean].describe === "Default TcA[T]")
    assert(TcA[Bar].describe === "gen(TcA[Int] :: Default TcA[T] :: HNil)")
    assert(TcA[Quux].describe ===
      "gen(TcA[Foo] :: gen(TcA[Int] :: Default TcA[T] :: HNil) :: TcA[Baz] :: HNil)")

    assert(TcB[Int].describe === "TcB[Int]")
    assert(TcB[Foo].describe === "gen(TcB[Int] :: HNil)")
    assert(TcB[Boolean].describe === "Default TcB[T]")
    assert(TcB[Bar].describe === "gen(TcB[Int] :: Default TcB[T] :: HNil)")
    assert(TcB[Quux].describe ===
      "gen(gen(TcB[Int] :: HNil) :: gen(TcB[Int] :: Default TcB[T] :: HNil) :: gen(Default TcB[T] :: HNil) :: HNil)")
  }

  test("Multi priority reexport") {
    import twopriorities._

    assert(TcA[Int].describe === "TcA[Int]")
    assert(TcA[Foo].describe === "TcA[Foo]")
    assert(TcA[Boolean].describe === "TcA[Boolean]")
    assert(TcA[Bar].describe === "TcA[Bar]")
    assert(TcA[Quux].describe ===
      "gen(TcA[Foo] :: TcA[Bar] :: TcA[Baz] :: HNil)")

    assert(TcB[Int].describe === "TcB[Int]")
    assert(TcB[Foo].describe === "Default TcB[T]")
    assert(TcB[Boolean].describe === "Default TcB[T]")
    assert(TcB[Bar].describe === "Default TcB[T]")
    assert(TcB[Quux].describe === "Default TcB[T]")
  }
}
