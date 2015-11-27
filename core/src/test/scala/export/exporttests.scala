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
  import tca._, deriver.tcaderived._

  test("TcA[Int]") {
    assert(TcA[Int].describe === "TcA[Int]")
  }

  test("TcA[Foo]") {
    assert(TcA[Foo].describe === "TcA[Foo]")
  }

  test("TcA[Bar]") {
    assert(TcA[Bar].describe === "gen(TcA[Int] :: Default TcA[T] :: HNil)")
  }

  test("TcA[Baz]") {
    assert(TcA[Baz].describe === "TcA[Baz]")
  }

  test("TcA[Double]") {
    assert(TcA[Double].describe === "Default TcA[T]")
  }

  test("TcA[Quux]") {
    assert(TcA[Quux].describe ===
      "gen(TcA[Foo] :: gen(TcA[Int] :: Default TcA[T] :: HNil) :: TcA[Baz] :: HNil)")
  }

  test("TcA[Option[Int]]") {
    assert(TcA[Option[Int]].describe ===
      "gen(gen(HNil) :+: gen(TcA[Int] :: HNil) :+: CNil)")
  }

  test("TcA[List[Int]]") {
    assert(TcA[List[Int]].describe ===
      "gen(gen(TcA[Int] :: loop (gen) :: HNil) :+: gen(HNil) :+: CNil)")
  }
}

class SubclassTests extends FunSuite {
  import tcb._, tcbext._

  test("TcB[Int]") {
    assert(TcB[Int].describe === "TcB[Int]")
  }

  test("TcB[Foo]") {
    import TcBSub.exports._
    assert(TcB[Foo].describe === "TcBSub[Foo]")
  }

  test("TcB[Bar]") {
    import TcBExtSub.exports._
    assert(TcB[Bar].describe === "TcBExtSub[Bar]")
  }
}

class OrphanTests extends FunSuite {
  import tca._, orphans.tcaorphans._, deriver.tcaderived._

  test("TcA[Int]") {
    assert(TcA[Int].describe === "TcA[Int]")
  }

  test("TcA[Foo]") {
    assert(TcA[Foo].describe === "TcA[Foo]")
  }

  test("TcA[Bar]") {
    assert(TcA[Bar].describe === "TcA[Bar]")
  }

  test("TcA[Baz]") {
    assert(TcA[Baz].describe === "TcA[Baz]")
  }

  test("TcA[Double]") {
    assert(TcA[Double].describe === "Default TcA[T]")
  }

  test("TcA[Boolean]") {
    assert(TcA[Boolean].describe === "TcA[Boolean]")
  }

  test("TcA[Quux]") {
    assert(TcA[Quux].describe ===
      "gen(TcA[Foo] :: TcA[Bar] :: TcA[Baz] :: HNil)")
  }
}

class InstantiationTests extends FunSuite {
  import tca._, instantiator.tc1instantiator._, deriver.tcaderived._

  test("TcA[Int]") {
    assert(TcA[Int].describe === "TcA[Int]")
  }

  test("TcA[Option[Int]]") {
    assert(TcA[Option[Int]].describe === "Instantiate TcA1[Option]")
  }

  test("TcA[List[Int]]") {
    assert(TcA[List[Int]].describe === "Instantiate Default TcA1[F]")
  }
}

class CustomPriorityTests extends FunSuite {
  import tca._, instantiator.tc1instantiator._, deriver.tcaderived._, customprioritization._

  test("TcA[Int]") {
    assert(TcA[Int].describe === "TcA[Int]")
  }

  test("TcA[Option[Int]]") {
    assert(TcA[Option[Int]].describe === "Instantiate TcA1[Option]")
  }

  import CustomPrioritization._

  test("TcA[List[Int]]") {
    assert(TcA[List[Int]].describe ===
      "gen(gen(TcA[Int] :: loop (gen) :: HNil) :+: gen(HNil) :+: CNil)")
  }
}

class TypeTests extends FunSuite {
  import tpedefns._

  test("Higher[List,Int]"){
    assert(Higher[List,Int].describe === "Higher[List, Int]")
  }

  test("Higher2[List, Int, Int]"){
    assert(Higher2[List,Int,Int].describe === "Higher2[List, Int, Int]")
  }

  test("Triple[Int, Int, Int]"){
    assert(Triple[Int,Int,Int].describe === "Triple[Int, Int, Int]")
  }
}