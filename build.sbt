val scala3Version = "3.0.0"

val commonSettings = Seq(
  version            := "0.1.0",
  scalaVersion       := scala3Version,
)

lazy val root = project
  .in(file("."))
  .settings(
    commonSettings,
    name := "LL1-Compile-Time",
    version := "0.1.0",

    scalaVersion := scala3Version,


    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    ),
  )

lazy val example = Project("example", file("example"))
  .settings(
    commonSettings,
    name := "example",
    scalaSource in Compile := baseDirectory.value,
  )
  .dependsOn(root)