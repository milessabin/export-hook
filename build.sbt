organization := "com.milessabin"

name := "derivation-hook"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.chuusai"    %% "shapeless"  % "2.2.5"  % "test",
  "org.scalatest"  %% "scalatest"  % "2.1.3"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
)

scalacOptions := Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked"
)
