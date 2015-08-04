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

package export

import scala.language.experimental.macros

import scala.annotation.StaticAnnotation

class Export0[F[_], T](val instance: F[T]) extends AnyVal

class Export1[F[_[_]], T[_]](val instance: F[T]) extends AnyVal

trait Exporter0[S[_]]  {
  implicit def exports[F[t] >: S[t], T](implicit st: S[T]): Export0[F, T] =
    macro ExportMacro.exportsImpl0[F, T]
}

trait Exporter1[S[_[_]]]  {
  implicit def exports[F[t[_]] >: S[t], T[_]](implicit st: S[T]): Export1[F, T] =
    macro ExportMacro.exportsImpl1[F, T]
}

class exported[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ExportMacro.exportedImpl
}
