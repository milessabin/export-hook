import sbt._

/**
 * Copied, with some modifications, from https://github.com/milessabin/shapeless/blob/master/project/Boilerplate.scala
 *
 * Generate a range of boilerplate classes, those offering alternatives with 0-22 params
 * and would be tedious to craft by hand
 *
 * @author Miles Sabin
 * @author Kevin Wright
 */


object Boilerplate {

  import scala.StringContext._

  implicit final class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map {
        _ dropWhile (_.isWhitespace)
      }
      trimmedLines mkString "\n"
    }
  }


  val testTemplates: Seq[Template] = Seq.empty
  val codeTemplates: Seq[Template] = Seq(ExportsImpl)

  def genTest(dir: File) = gen(dir)(testTemplates)
  def genCode(dir: File) = gen(dir)(codeTemplates)

  val header = "// auto-generated boilerplate" // TODO: put something meaningful here?


  /** Returns a seq of the generated files.  As a side-effect, it actually generates them... */
  def gen(dir: File)(templates: Seq[Template]) = for (t <- templates) yield {
    val tgtFile = t.filename(dir)
    IO.write(tgtFile, t.body)
    tgtFile
  }

  val maxArity = 22

  final class TemplateVals(val arity: Int, prefix: String = "A") {
    private val a = prefix.toLowerCase

    val synTypes = (0 until arity) map (n => s"$prefix$n")
    val synVals = (0 until arity) map (n => s"$a$n")
    val synTypedVals = (synVals zip synTypes) map { case (v, t) => v + ":" + t }
    val `A..N` = synTypes.mkString(", ")
    val `a..n` = synVals.mkString(", ")
    val `_.._` = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)` = if (arity == 1) s"Tuple1[$prefix]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)` = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)` = if (arity == 1) s"Tuple1($a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N` = synTypedVals mkString ", "
  }

  trait Template {
    def filename(root: File): File
    def content(tv: TemplateVals): String
    def range = 1 to maxArity
    def body: String = {
      val headerLines = header split '\n'
      val rawContents = range map { n => content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty) }
      val preBody = rawContents.head takeWhile (_ startsWith "|") map (_.tail)
      val instances = rawContents flatMap {
        _ filter (_ startsWith "-") map (_.tail)
      }
      val postBody = rawContents.head dropWhile (_ startsWith "|") dropWhile (_ startsWith "-") map (_.tail)
      (headerLines ++ preBody ++ instances ++ postBody) mkString "\n"
    }
  }

  /*
    Blocks in the templates below use a custom interpolator, combined with post-processing to produce the body
      - The contents of the `header` val is output first
      - Then the first block of lines beginning with '|'
      - Then the block of lines beginning with '-' is replicated once for each arity,
        with the `templateVals` already pre-populated with relevant relevant vals for that arity
      - Then the last block of lines prefixed with '|'
    The block otherwise behaves as a standard interpolated string with regards to variable substitution.
  */

  object ExportsImpl extends Template{
    override def filename(root: File): File = root / "export" / "exportGen.scala"

    override def content(tv: TemplateVals): String = {
      import tv._

      val tcTags = synVals map (_ + "Tag")

      val tc0Code = List.fill(arity)("0") mkString ""
      val tc0 = if(arity > 1) List.fill(arity)("_") mkString ("TC[", ",", "]") else "TC[_]"
      val tc1Tpe = tcTags.map(_ + ".tpe") mkString ("List(", ",", ")")
      val tc0Implicits = (tcTags zip synTypes) map {
        case (tag, st) => s"$tag: WeakTypeTag[$st]"
      } mkString ","

      val tc1Code = List.fill(arity)("0").tail.mkString("1", "", "")
      val tc1 = if(arity > 1) List.fill(arity)("_").tail.mkString("TC[_[_],", ",", "]") else "TC[_[_]]"
      val tc1Types = ("HK[_]" :: synTypes.toList.tail).mkString(",")
      val tc1Weak = if(arity > 1) List.fill(arity)("_").tail.mkString("TC[Any,", ",", "]") else "TC[Any]"
      val tc1Implicits = (("hkTag", "HK[_]") :: (tcTags zip synTypes).toList.tail).map{
        case (tag, st) => s"$tag: WeakTypeTag[$st]"
      }.mkString(",")
      val tc1TL = ("hkTag" :: tcTags.toList.tail).map(_ + ".tpe").mkString("List(", ",", ")")

      block"""
        |package export
        |
        |import scala.language.experimental.macros
        |import scala.reflect.macros.whitebox
        |import macrocompat.bundle
        |
        |@bundle
        |class ExportsImplExpr(val c: whitebox.Context) {
        |  import c.universe._ 
        |
        -  def exportsImpl$tc0Code[$tc0, ${`A..N`}, E[_]]
        -    (implicit tcTag: WeakTypeTag[$tc0], $tc0Implicits, eTag: WeakTypeTag[E[_]]) =
        -      exportsImplAux(tcTag.tpe.typeConstructor, $tc1Tpe, eTag.tpe)
        -
        -  def exportsImpl$tc1Code[$tc1, $tc1Types, E[_]]
        -    (implicit tcTag: WeakTypeTag[$tc1Weak], $tc1Implicits, eTag: WeakTypeTag[E[_]]) =
        -      exportsImplAux(tcTag.tpe.typeConstructor, $tc1TL, eTag.tpe)
        -
        |  def exportsImplAux(tcTpe: Type, tTpes: List[Type], eTpe: Type): Tree = {
        |    val appTpe = appliedType(tcTpe, tTpes)
        |    val st =
        |      c.typecheck(q"_root_.shapeless.lazily[$$appTpe]", silent = true) match {
        |        case EmptyTree =>
        |          c.typecheck(q"_root_.scala.Predef.implicitly[$$appTpe]", silent = true) match {
        |            case EmptyTree =>
        |              c.abort(c.enclosingPosition, s"Unable to infer value of type $$appTpe")
        |            case t => t
        |          }
        |        case t => t
        |    }
        |    val exportTc = eTpe.typeConstructor
        |    val exportTpe = appliedType(exportTc, appliedType(tcTpe, tTpes))
        |    q"new $$exportTpe($$st)"
        |  }
        |}
      """
    }
  }
}