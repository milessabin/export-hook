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

 package tpedefns

import export.{export, exports, imports}
import org.scalatest.FunSuite

trait Higher0[M[_]]{
  def describe: String
}

@exports
object Higher0 extends LowerPriorityHigher0{
  def apply[M[_]](implicit h: Higher0[M]) = h

  implicit def l = new Higher0[List]{
    def describe = "Higher[List]"
  }
}

@imports[Higher0]
trait LowerPriorityHigher0

trait Higher1[M[_], A]{
  def describe: String
}

@exports
object Higher1 extends LowerPriorityHigher1{
  def apply[M[_], A](implicit h: Higher1[M, A]) = h

  @export
  implicit def lint = new Higher1[List, Int]{
    def describe: String = "Higher[List, Int]"
  }
}

@imports[Higher1]
trait LowerPriorityHigher1

trait Higher2[M[_], A, B]{
  def describe: String
}

@exports
object Higher2 extends LowerPriorityHigher2{
  def apply[M[_], A, B](implicit h: Higher2[M, A, B]) = h

  @export
  implicit def lint = new Higher2[List, Int, Int]{
    def describe: String = "Higher2[List, Int, Int]"
  }
}

@imports[Higher2]
trait LowerPriorityHigher2

trait Higher5[M[_], A, B, C, D, E]{
  def describe: String
}

@exports
object Higher5 extends LowerPriorityHigher5{
  def apply[M[_], A, B, C, D, E](implicit h: Higher5[M, A, B, C, D, E]) = h

  @export
  implicit def lint = new Higher5[List, Int, Int, Int, Int, Int]{
    def describe: String = "Higher5[List, Int, Int, Int, Int, Int]"
  }
}

trait LowerPriorityHigher5

trait Triple[A, B, C]{
  def describe: String
}

@exports
object Triple extends LowerPriorityTriple{
  def apply[A, B, C](implicit trp: Triple[A, B, C]) = trp

  @export
  implicit def iii = new Triple[Int, Int, Int]{
    def describe: String = "Triple[Int, Int, Int]"
  }
}

@imports[Triple]
trait LowerPriorityTriple

class TypeTests extends FunSuite {

  test("Higher0[List]"){
    assert(Higher0[List].describe === "Higher[List]")
  }

  test("Higher1[List,Int]"){
    assert(Higher1[List,Int].describe === "Higher[List, Int]")
  }

  test("Higher2[List, Int, Int]"){
    assert(Higher2[List,Int,Int].describe === "Higher2[List, Int, Int]")
  }

  test("Higher5[List,Int,Int,Int,Int,Int,Int]"){
    assert(Higher5[List,Int,Int,Int,Int,Int].describe === "Higher5[List, Int, Int, Int, Int, Int]")
  }

  test("Triple[Int, Int, Int]"){
    assert(Triple[Int,Int,Int].describe === "Triple[Int, Int, Int]")
  }
}