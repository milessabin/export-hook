lazy val buildSettings = Seq(
  organization := "org.typelevel",
  scalaVersion := "2.11.7"
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
    "com.chuusai"    %% "shapeless"  % "2.2.5"  % "test",
    "org.scalatest"  %% "scalatest"  % "2.1.3"  % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",

    "org.scala-lang" %  "scala-reflect" % scalaVersion.value % "provided",

    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")
  ),
  scmInfo := Some(ScmInfo(url("https://github.com/milessabin/export-hook"),
    "scm:git:git@github.com:milessabin/export-hook.git"))
)

lazy val exportHookSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val exportHook = project.in(file("."))
  .settings(moduleName := "export-hook")
  .settings(exportHookSettings)

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

addCommandAlias("validate", ";compile;test")

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
