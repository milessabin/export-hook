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

package alleycats

import export.{ exports, export, imports, reexports }
import scala.util.Try
import simulacrum.typeclass

@typeclass
trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Monad extends MonadInstances {
  implicit def listMonad: Monad[List] =
    new Monad[List] {
      def pure[A](a: A): List[A] = List(a)
      def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }
}

@imports[Monad]
trait MonadInstances

@reexports(TryMonad)
object try_

@exports
object TryMonad {
  @export(Orphan)
  implicit val tryMonad: Monad[Try] =
    new Monad[Try] {
      def pure[A](a: A): Try[A] = Try(a)
      def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
    }
}

@typeclass
trait Empty[A] {
  def empty: A
}

object Empty extends EmptyInstances {
  def apply[A](a: => A): Empty[A] =
    new Empty[A] { lazy val empty: A = a }

  implicit def emptyInt: Empty[Int] = Empty(0)
}

@imports[Empty]
trait EmptyInstances

@typeclass trait EmptyK[F[_]] { self =>
  def empty[A]: F[A]

  def synthesize[A]: Empty[F[A]] =
    new Empty[F[A]] {
      def empty: F[A] = self.empty[A]
    }
}

@imports[EmptyK]
object EmptyK {
  implicit def listEmptyK: EmptyK[List] =
    new EmptyK[List] {
      def empty[A]: List[A] = Nil
    }
}

@exports
object InstantiatedEmptyK {
  @export(Instantiated)
  implicit def instantiate[F[_], T](implicit ekf: EmptyK[F]): Empty[F[T]] = ekf.synthesize[T]
}

@reexports(InstantiatedEmptyK)
object emptykinst

trait Foo[T]

@imports[Foo]
object Foo

object TestExports {
  Monad[List]

  import try_._
  Monad[Try]

  Empty[Int]
  EmptyK[List]

  import emptykinst._
  Empty[List[Int]]
}
