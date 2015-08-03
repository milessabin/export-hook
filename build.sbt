organization := "com.milessabin"

name := "derivation-hook"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

libraryDependencies ++= Seq(
  "com.chuusai"    %% "shapeless"  % "2.2.5"  % "test",
  "org.scalatest"  %% "scalatest"  % "2.1.3"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",

  "org.scala-lang" %  "scala-reflect" % scalaVersion.value % "provided",

  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4")
)

scalacOptions := Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked"
)
