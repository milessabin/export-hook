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
import scala.reflect.macros.Context

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

class ExportMacro[C <: Context](val c: C) {
  import c.universe._

  def exportsImpl0[F[_], T](st: Tree)
    (implicit fTag: WeakTypeTag[F[_]], tTag: WeakTypeTag[T]): Tree = {
    val fTpe = fTag.tpe.typeConstructor
    val tTpe = tTag.tpe
    q"""new _root_.export.Export0[$fTpe, $tTpe]($st)"""
  }

  def exportsImpl1[F[_[_]], T[_]](st: Tree)
    (implicit fTag: WeakTypeTag[F[Any]], tTag: WeakTypeTag[T[_]]): Tree = {
    val fTpe = fTag.tpe.typeConstructor
    val tTpe = tTag.tpe.typeConstructor
    q"""new _root_.export.Export1[$fTpe, $tTpe]($st)"""
  }

  def exportedImpl(annottees: Tree*): Tree = {
    val Apply(Select(New(AppliedTypeTree(_, List(tc))), _), _) = c.prefix.tree
    val tcTpe = c.typeCheck(q"null.asInstanceOf[$tc[Any]]", silent = true).tpe.typeConstructor
    val kind = tcTpe.typeSymbol.asType.typeParams.head.asType.typeParams.length
    if(kind > 1)
      c.abort(c.enclosingPosition, "$tc has an unsupported kind")

    annottees match {
      case List(ClassDef(mods, typeName, List(), impl)) =>
        val lpName = newTypeName(c.fresh)
        val lpClass = ClassDef(mods, lpName, List(), impl)
        val termName = typeName.toTermName
        val methName = newTermName(c.fresh)
        val t = newTypeName(c.fresh)
        val d = newTermName(c.fresh)

        val td = if(kind == 0) q"type $t" else q"type $t[_]"
        val importImpl = newTermName(s"importImpl$kind")

        q"""
          trait $typeName extends $termName.$lpName {
            import scala.language.experimental.macros
            implicit def $methName[$td]: $tc[$t] = macro _root_.export.ExportMacro.$importImpl[$tc, $t]
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
    importImplAux(fTag.tpe.typeConstructor, tTag.tpe, typeOf[Export0[Any, _]].typeConstructor)

  def importImpl1[F[_[_]], T[_]](implicit fTag: WeakTypeTag[F[Any]], tTag: WeakTypeTag[T[_]]): Tree =
    importImplAux(fTag.tpe.typeConstructor, tTag.tpe, typeOf[Export1[Any, Any]].typeConstructor)

  def importImplAux(fTpe: Type, tTpe: Type, eTpe: Type): Tree = {
    val appTpe = appliedType(eTpe, List(fTpe, tTpe))

    val exporter = c.inferImplicitValue(appTpe, silent = true)
    if(exporter == EmptyTree)
      c.abort(c.enclosingPosition, s"Unable to find export of type $appTpe")

    q"""$exporter.instance"""
  }
}


object ExportMacro {
  def inst(c: Context) = new ExportMacro[c.type](c)

  def exportsImpl0[F[_], T](c: Context)(st: c.Expr[F[T]])
    (implicit fTag: c.WeakTypeTag[F[_]], tTag: c.WeakTypeTag[T]): c.Expr[Export0[F, T]] =
      c.Expr[Export0[F, T]](inst(c).exportsImpl0[F, T](st.tree))

  def exportsImpl1[F[_[_]], T[_]](c: Context)(st: c.Expr[F[T]])
    (implicit fTag: c.WeakTypeTag[F[Any]], tTag: c.WeakTypeTag[T[_]]): c.Expr[Export1[F, T]] =
      c.Expr[Export1[F, T]](inst(c).exportsImpl1[F, T](st.tree))

  def exportedImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    c.Expr[Any](inst(c).exportedImpl(annottees.map(_.tree): _*))

  def importImpl0[F[_], T](c: Context)
    (implicit fTag: c.WeakTypeTag[F[_]], tTag: c.WeakTypeTag[T]): c.Expr[F[T]] =
      c.Expr[F[T]](inst(c).importImpl0[F, T])

  def importImpl1[F[_[_]], T[_]](c: Context)
    (implicit fTag: c.WeakTypeTag[F[Any]], tTag: c.WeakTypeTag[T[_]]): c.Expr[F[T]] =
      c.Expr[F[T]](inst(c).importImpl1[F, T])
}
