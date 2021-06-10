val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "LL1-Compile-Time",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )

lazy val example = Project("example", file("example"))
  .settings(
    name := "example",
    scalaSource in Compile := baseDirectory.value,
    scalaVersion := scala3Version
  )
  .dependsOn(root)