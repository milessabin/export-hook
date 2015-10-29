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

import macrocompat.bundle

// https://issues.scala-lang.org/browse/SI-7332
case class Export0[+T](instance: T) extends AnyVal
case class Export1[+T](instance: T) extends AnyVal
case class Export2[+T](instance: T) extends AnyVal
case class Export3[+T](instance: T) extends AnyVal
case class Export4[+T](instance: T) extends AnyVal
case class Export5[+T](instance: T) extends AnyVal
case class Export6[+T](instance: T) extends AnyVal
case class Export7[+T](instance: T) extends AnyVal

trait ExportPriority0
class ExportPriority[E0[_], E1[_], E2[_], E3[_], E4[_], E5[_], E6[_], E7[_]] extends ExportPriority0
object ExportPriority {
  def apply[E0[_], E1[_], E2[_], E3[_], E4[_], E5[_], E6[_], E7[_]] =
    new ExportPriority[E0, E1, E2, E3, E4, E5, E6, E7]
}

class exports extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ExportMacro.exportsImpl
}

class reexports extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ExportMacro.reexportsImpl
}

class imports[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ExportMacro.importsImpl
}

@bundle
class ExportMacro(val c: whitebox.Context) {
  import c.universe._

  val exportTcs =
    List(
      typeOf[Export0[_]].typeConstructor,
      typeOf[Export1[_]].typeConstructor,
      typeOf[Export2[_]].typeConstructor,
      typeOf[Export3[_]].typeConstructor,
      typeOf[Export4[_]].typeConstructor,
      typeOf[Export5[_]].typeConstructor,
      typeOf[Export6[_]].typeConstructor,
      typeOf[Export7[_]].typeConstructor
    )

  val defaultPriority = 5

  object Priority {
    def unapply(s: String): Option[Int] = {
      s match {
        case "HighPriority" => Some(0)
        case "Orphan" => Some(1)
        case "Subclass" => Some(2)
        case "Algebraic" => Some(3)
        case "Instantiated" => Some(4)
        case "Generic" => Some(5)
        case "Default" => Some(6)
        case "LowPriority" => Some(7)
        case _ => None
      }
    }
  }

  def mkAttributedRef(tpe: Type): Tree = {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    val gTpe = tpe.asInstanceOf[global.Type]
    val pre = gTpe.prefix
    val sym = gTpe.typeSymbol
    global.gen.mkAttributedRef(pre, sym).asInstanceOf[Tree]
  }

  def mkAnonImplicit(tcTpe: Type, exportTc: Type, prefix: String, index: Int): Tree = {
    val tcRef = mkAttributedRef(tcTpe)

    val kind = tcTpe.typeParams.map(_.asType.typeParams.length)
    val suffix =
      kind match {
        case List(0) => "0"
        case List(1) => "1"
        case List(0, 0) => "00"
        case _ =>
          c.abort(c.enclosingPosition, "$tc has an unsupported kind")
      }
    val (f, fd) = {
      val t = TypeName(c.freshName)
      val x = TypeName(c.freshName)
      val x1 = TypeName(c.freshName)
      val y = TypeName(c.freshName)
      (
        t,
        kind match {
          case List(0) => q"type $t[$x] >: $tcRef[$x]"
          case List(1) => q"type $t[$x1[_]] >: $tcRef[$x1]"
          case List(0, 0) => q"type $t[$x, $y] >: $tcRef[$x, $y]"
          case _ =>
            c.abort(c.enclosingPosition, "$tc has an unsupported kind")
        }
        )
    }

    val (ts: List[TypeName], tds: List[Tree]) = (kind.map { k =>
      val t = TypeName(c.freshName)
      (t, if(k == 0) q"type $t" else q"type $t[_]")
    }).unzip

    val exportTpt = c.internal.gen.mkAttributedRef(exportTc.typeSymbol)
    val exportsImpl = TermName(s"exportsImpl$suffix")
    val implicitName = TermName(prefix+"$anonimplicit$"+index)

    q"""
    implicit def $implicitName[$fd, ..$tds]: $exportTpt[$f[..$ts]] =
      macro _root_.export.ExportMacro.$exportsImpl[$tcRef, ..$ts, $exportTpt]
      """
  }

  def exportsImpl(annottees: Tree*): Tree = {
    annottees match {
      case List(m @ ModuleDef(mods, termName, Template(parents, self, body0))) =>
        val priority =
          c.prefix.tree match {
            case Apply(_, List(Literal(Constant(i: Int)))) => i
            case Apply(_, List(Ident(TermName(Priority(i))))) => i
            case _ => defaultPriority
          }

        val body = body0.filter {
          case DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => false
          case _ => true
        }

        val tc = Ident(termName.toTypeName)
        val tcTpe = c.typecheck(tc, c.TYPEmode).tpe.typeConstructor
        val prefix = c.internal.enclosingOwner.fullName.replace('.', '$')+"$$"+termName
        val anonImplicit = mkAnonImplicit(tcTpe, exportTcs(priority), prefix, 0)

        q"""
          $mods object $termName extends ..$parents { $self =>
            object exports {
              import scala.language.experimental.macros

              $anonImplicit
            }
            ..$body
          }
        """

      case other =>
        c.abort(c.enclosingPosition, "Unexpected tree shape.")
    }
  }

  def reexportsImpl(annottees: Tree*): Tree = {
    annottees match {
      case List(m @ ModuleDef(mods, termName, Template(parents, self, body0))) =>
        val body = body0.filter {
          case DefDef(_, termNames.CONSTRUCTOR, _, _, _, _) => false
          case _ => true
        }

        val prefix = c.internal.enclosingOwner.fullName.replace('.', '$')+"$$"+termName

        val tcs =
          c.prefix.tree match {
            case Apply(Select(New(AppliedTypeTree(_, tcs)), _), _) => tcs.zipWithIndex
            case _ => c.abort(c.enclosingPosition, "No reexports specified.")
          }

        val anonImplicits =
          for {
            (tc, index) <- tcs
            tcTpe = c.typecheck(tc, c.TYPEmode, silent = true).tpe.typeConstructor if !(tcTpe =:= NoType)
            cTpe = tcTpe.companion
            eTpe = cTpe.decl(TermName("exports")).infoIn(cTpe)
            mSym <- eTpe.decls if !mSym.isConstructor
            PolyType(List(pSym, _), NullaryMethodType(rTpe)) = mSym.infoIn(eTpe)
            PolyType(_, TypeBounds(lo, _)) = pSym.infoIn(eTpe)
          } yield mkAnonImplicit(lo.typeConstructor, rTpe.typeConstructor, prefix, index)

        q"""
          $mods object $termName extends ..$parents { $self =>
            import scala.language.experimental.macros

            ..$anonImplicits

            ..$body
          }
        """

      case other =>
        c.abort(c.enclosingPosition, "Unexpected tree shape.")
    }
  }

  def exportsImpl0[TC[_], T, E[_]]
    (implicit tcTag: WeakTypeTag[TC[_]], tTag: WeakTypeTag[T], eTag: WeakTypeTag[E[_]]): Tree =
    exportsImplAux(tcTag.tpe.typeConstructor, List(tTag.tpe), eTag.tpe)

  def exportsImpl1[TC[_[_]], T[_], E[_]]
    (implicit tcTag: WeakTypeTag[TC[Any]], tTag: WeakTypeTag[T[_]], eTag: WeakTypeTag[E[_]]): Tree =
    exportsImplAux(tcTag.tpe.typeConstructor, List(tTag.tpe.typeConstructor), eTag.tpe)

  def exportsImpl00[TC[_, _], T, U, E[_]]
    (implicit tcTag: WeakTypeTag[TC[_, _]], tTag: WeakTypeTag[T], uTag: WeakTypeTag[U], eTag: WeakTypeTag[E[_]]): Tree =
    exportsImplAux(tcTag.tpe.typeConstructor, List(tTag.tpe, uTag.tpe), eTag.tpe)

  def exportsImplAux(tcTpe: Type, tTpes: List[Type], eTpe: Type): Tree = {
    val appTpe = appliedType(tcTpe, tTpes)
    val st =
      c.typecheck(q"_root_.shapeless.lazily[$appTpe]", silent = true) match {
        case EmptyTree =>
          c.typecheck(q"_root_.scala.Predef.implicitly[$appTpe]", silent = true) match {
            case EmptyTree =>
              c.abort(c.enclosingPosition, s"Unable to infer value of type $appTpe")
            case t => t
          }
        case t => t
      }

    val exportTc = eTpe.typeConstructor
    val exportTpe = appliedType(exportTc, appliedType(tcTpe, tTpes))
    q"""new $exportTpe($st)"""
  }

  def importsImpl(annottees: Tree*): Tree = {
    val Apply(Select(New(AppliedTypeTree(_, List(tc))), _), _) = c.prefix.tree
    val tcTpe = c.typecheck(tc, c.TYPEmode).tpe.typeConstructor
    val tcRef = mkAttributedRef(tcTpe)
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
            implicit def $methName[..$tds]: $tcRef[..$ts] = macro _root_.export.ExportMacro.$importImpl[$tcRef, ..$ts]
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
    importImplAux(fTag.tpe.typeConstructor, List(tTag.tpe))

  def importImpl1[F[_[_]], T[_]](implicit fTag: WeakTypeTag[F[Any]], tTag: WeakTypeTag[T[_]]): Tree =
    importImplAux(fTag.tpe.typeConstructor, List(tTag.tpe))

  def importImpl00[F[_, _], T, U](implicit fTag: WeakTypeTag[F[_, _]], tTag: WeakTypeTag[T], uTag: WeakTypeTag[U]): Tree =
    importImplAux(fTag.tpe.typeConstructor, List(tTag.tpe, uTag.tpe))

  def importImplAux(fTpe: Type, tTpes: List[Type]): Tree = {
    val instTpe = appliedType(fTpe, tTpes)
    object Resolved {
      def unapply(tc: Type): Option[Tree] = {
        val appTpe = appliedType(tc, instTpe)
        c.inferImplicitValue(appTpe, silent = true) match {
          case EmptyTree => None
          case t => Some(t)
        }
      }
    }

    val orderedTcs =
      c.inferImplicitValue(typeOf[ExportPriority0], silent = true) match {
        case EmptyTree => exportTcs
        case t =>
          val TypeRef(_, _, args) = t.tpe
          args
      }

    val resolved = orderedTcs.collectFirst { case Resolved(t) => t }
    resolved match {
      case Some(exporter) =>
        q"""$exporter.instance"""
      case None =>
        c.abort(c.enclosingPosition, s"Unable to find export of type $instTpe")
    }
  }
}
