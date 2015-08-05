import org.scalajs.sbtplugin.cross.CrossProject

lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7")
)

lazy val commonSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked"
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    "bintray/non" at "http://dl.bintray.com/non/maven"
  ),
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")
  ),

  libraryDependencies += "com.chuusai"    %%% "shapeless"  % "2.2.5"  % "test",
  libraryDependencies += "org.scalatest"  %%% "scalatest"  % "3.0.0-M7"  % "test",
  libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.12.4" % "test",

  scmInfo := Some(ScmInfo(url("https://github.com/milessabin/export-hook"),
    "scm:git:git@github.com:milessabin/export-hook.git"))
) ++ crossVersionSharedSources ++ scalaMacroDependencies

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution in Test := false
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false
)

lazy val exportHookSettings = buildSettings ++ commonSettings ++ publishSettings

onLoad in Global := (onLoad in Global).value andThen (Command.process(s"project exportHookJVM", _: State))

lazy val root = project.in(file("."))
  .aggregate(exportHookJS, exportHookJVM)
  .dependsOn(exportHookJS, exportHookJVM)
  .settings(exportHookSettings:_*)
  .settings(noPublishSettings)

lazy val exportHook = crossProject.crossType(CrossType.Pure)
  .settings(moduleName := "export-hook")
  .settings(exportHookSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val exportHookJVM = exportHook.jvm
lazy val exportHookJS = exportHook.js

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/milessabin/export-hook")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>non</id>
        <name>Miles Sabin</name>
        <url>http://milessabin.com/blog</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

addCommandAlias("validate", ";root/compile;root/test")

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
              "org.scalamacros" %% "quasiquotes" % "2.0.0" cross CrossVersion.binary
        )
    }
  }
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
