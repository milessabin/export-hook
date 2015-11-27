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

import export.{export, exports}

trait Higher[M[_], A]{
  def describe: String
}

@exports
object Higher{
  def apply[M[_], A](implicit h: Higher[M, A]) = h

  @export
  implicit def lint = new Higher[List, Int]{
    def describe: String = "Higher[List, Int]"
  }
}

trait Higher2[M[_], A, B]{
  def describe: String
}

@exports
object Higher2{
  def apply[M[_], A, B](implicit h: Higher2[M, A, B]) = h

  @export
  implicit def lint = new Higher2[List, Int, Int]{
    def describe: String = "Higher2[List, Int, Int]"
  }
}

trait Triple[A, B, C]{
  def describe: String
}

@exports
object Triple{
  def apply[A, B, C](implicit trp: Triple[A, B, C]) = trp

  @export
  implicit def iii = new Triple[Int, Int, Int]{
    def describe: String = "Triple[Int, Int, Int]"
  }
}