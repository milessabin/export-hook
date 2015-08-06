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
import scala.reflect.macros.whitebox

class Export0[F[_], T](val instance: F[T]) extends AnyVal

class Export1[F[_[_]], T[_]](val instance: F[T]) extends AnyVal

class Export00[F[_, _], T, U](val instance: F[T, U]) extends AnyVal

trait Exporter0[S[_]]  {
  implicit def exports[F[t] >: S[t], T](implicit st: S[T]): Export0[F, T] =
    macro ExportMacro.exportsImpl0[F, T]
}

trait Exporter1[S[_[_]]]  {
  implicit def exports[F[t[_]] >: S[t], T[_]](implicit st: S[T]): Export1[F, T] =
    macro ExportMacro.exportsImpl1[F, T]
}

trait Exporter00[S[_, _]]  {
  implicit def exports[F[t, u] >: S[t, u], T, U](implicit st: S[T, U]): Export00[F, T, U] =
    macro ExportMacro.exportsImpl00[F, T, U]
}

class exported[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ExportMacro.exportedImpl
}

// For 2.11.x only we would have just,
// class ExportMacro(val c: whitebox.Context) {
class ExportMacro0(val c: whitebox.Context) extends MacroCompat {
  import c.universe._

  def exportsImpl0[F[_], T](st: Tree)
    (implicit fTag: WeakTypeTag[F[_]], tTag: WeakTypeTag[T]): Tree =
    exportsImplAux(fTag.tpe.typeConstructor, List(tTag.tpe), typeOf[Export0[Any, _]].typeConstructor, st)

  def exportsImpl1[F[_[_]], T[_]](st: Tree)
    (implicit fTag: WeakTypeTag[F[Any]], tTag: WeakTypeTag[T[_]]): Tree =
    exportsImplAux(fTag.tpe.typeConstructor, List(tTag.tpe.typeConstructor), typeOf[Export1[Any, Any]].typeConstructor, st)

  def exportsImpl00[F[_, _], T, U](st: Tree)
    (implicit fTag: WeakTypeTag[F[_, _]], tTag: WeakTypeTag[T], uTag: WeakTypeTag[U]): Tree =
    exportsImplAux(fTag.tpe.typeConstructor, List(tTag.tpe, uTag.tpe), typeOf[Export00[Any, _, _]].typeConstructor, st)

  def exportsImplAux(fTpe: Type, tTpes: List[Type], eTpe: Type, st: Tree): Tree = {
    val appTpe = appliedType(eTpe, fTpe :: tTpes)
    q"""new $appTpe($st)"""
  }

  def exportedImpl(annottees: Tree*): Tree = {
    val Apply(Select(New(AppliedTypeTree(_, List(tc))), _), _) = c.prefix.tree
    val tcTpe = c.typecheck(tc, c.TYPEmode).tpe
    val kind = tcTpe.typeParams.map(_.asType.typeParams.length)
    val suffix =
      kind match {
        case List(0) => "0"
        case List(1) => "1"
        case List(0, 0) => "00"
        case _ =>
          c.abort(c.enclosingPosition, "$tc has an unsupported kind")
      }

    annottees match {
      case List(ClassDef(mods, typeName, List(), impl)) =>
        val lpName = TypeName(c.freshName)
        val lpClass = ClassDef(mods, lpName, List(), impl)
        val termName = typeName.toTermName
        val methName = TermName(c.freshName)
        val (ts: List[TypeName], tds: List[Tree]) = (kind.map { k =>
          val t = TypeName(c.freshName)
          (t, if(k == 0) q"type $t" else q"type $t[_]")
        }).unzip

        val importImpl = TermName(s"importImpl$suffix")

        q"""
          trait $typeName extends $termName.$lpName {
            import scala.language.experimental.macros
            implicit def $methName[..$tds]: $tc[..$ts] = macro _root_.export.ExportMacro.$importImpl[$tc, ..$ts]
          }
          object $termName {
            $lpClass
          }
        """

      case other =>
        c.abort(c.enclosingPosition, "Unexpected tree shape.")
    }
  }

  def importImpl0[F[_], T](implicit fTag: WeakTypeTag[F[_]], tTag: WeakTypeTag[T]): Tree =
    importImplAux(fTag.tpe.typeConstructor, List(tTag.tpe), typeOf[Export0[Any, _]].typeConstructor)

  def importImpl1[F[_[_]], T[_]](implicit fTag: WeakTypeTag[F[Any]], tTag: WeakTypeTag[T[_]]): Tree =
    importImplAux(fTag.tpe.typeConstructor, List(tTag.tpe), typeOf[Export1[Any, Any]].typeConstructor)

  def importImpl00[F[_, _], T, U](implicit fTag: WeakTypeTag[F[_, _]], tTag: WeakTypeTag[T], uTag: WeakTypeTag[U]): Tree =
    importImplAux(fTag.tpe.typeConstructor, List(tTag.tpe, uTag.tpe), typeOf[Export00[Any, _, _]].typeConstructor)

  def importImplAux(fTpe: Type, tTpes: List[Type], eTpe: Type): Tree = {
    val appTpe = appliedType(eTpe, fTpe :: tTpes)

    val exporter = c.inferImplicitValue(appTpe, silent = true)
    if(exporter == EmptyTree)
      c.abort(c.enclosingPosition, s"Unable to find export of type $appTpe")

    q"""$exporter.instance"""
  }
}
