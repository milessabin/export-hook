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

package scala.reflect.macros {
  object whitebox {
    type Context = scala.reflect.macros.Context
  }
}

package export {
  import scala.language.experimental.macros

  import scala.annotation.StaticAnnotation
  import scala.reflect.macros.whitebox

  trait MacroCompat {
    val c: whitebox.Context
    import c.universe._

    implicit def mkContextOps(c0: c.type): this.type = this

    def TypeName(s: String) = newTypeName(s)
    def TermName(s: String) = newTermName(s)
    def freshName = c.fresh

    sealed trait TypecheckMode
    case object TERMmode extends TypecheckMode
    case object TYPEmode extends TypecheckMode

    def typecheck(
      tree: Tree,
      mode: TypecheckMode = TERMmode,
      pt: Type = WildcardType,
      silent: Boolean = false,
      withImplicitViewsDisabled: Boolean = false,
      withMacrosDisabled: Boolean = false
    ): Tree =
      mode match {
        case TERMmode =>
          c.typeCheck(tree, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)
        case TYPEmode =>
          val term = q"null.asInstanceOf[$tree[Any]]"
          c.typeCheck(term, pt, silent, withImplicitViewsDisabled, withMacrosDisabled)
      }

    implicit def mkTypeOps(tpe: Type): TypeOps = new TypeOps(tpe)
    class TypeOps(tpe: Type) {
      def typeParams = tpe.typeSymbol.asType.typeParams
    }

    def appliedType(tc: Type, ts: List[Type]): Type = c.universe.appliedType(tc, ts)
    def appliedType(tc: Type, t: Type): Type = c.universe.appliedType(tc, List(t))
  }

  object ExportMacro {
    import whitebox.Context

    class ExportMacro[C <: Context](override val c: C) extends ExportMacro0(c)
    def inst[C <: Context](c: C) = new ExportMacro[c.type](c)

    def exportsImpl0[F[_], T](c: Context)(st: c.Expr[F[T]])
      (implicit fTag: c.WeakTypeTag[F[_]], tTag: c.WeakTypeTag[T]): c.Expr[Export[F[T]]] =
        c.Expr[Export[F[T]]](inst(c).exportsImpl0[F, T](st.tree))

    def exportsImpl1[F[_[_]], T[_]](c: Context)(st: c.Expr[F[T]])
      (implicit fTag: c.WeakTypeTag[F[Any]], tTag: c.WeakTypeTag[T[_]]): c.Expr[Export[F[T]]] =
        c.Expr[Export[F[T]]](inst(c).exportsImpl1[F, T](st.tree))

    def exportsImpl00[F[_, _], T, U](c: Context)(st: c.Expr[F[T, U]])
      (implicit fTag: c.WeakTypeTag[F[_, _]], tTag: c.WeakTypeTag[T], uTag: c.WeakTypeTag[U]): c.Expr[Export[F[T, U]]] =
        c.Expr[Export[F[T, U]]](inst(c).exportsImpl00[F, T, U](st.tree))

    def exportedImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
      c.Expr[Any](inst(c).exportedImpl(annottees.map(_.tree): _*))

    def importImpl0[F[_], T](c: Context)
      (implicit fTag: c.WeakTypeTag[F[_]], tTag: c.WeakTypeTag[T]): c.Expr[F[T]] =
        c.Expr[F[T]](inst(c).importImpl0[F, T])

    def importImpl1[F[_[_]], T[_]](c: Context)
      (implicit fTag: c.WeakTypeTag[F[Any]], tTag: c.WeakTypeTag[T[_]]): c.Expr[F[T]] =
        c.Expr[F[T]](inst(c).importImpl1[F, T])
  }
}
