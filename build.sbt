val scala3Version = "3.0.0"

val commonSettings = Seq(
  version            := "0.1.0",
  scalaVersion       := scala3Version,
)

lazy val root = project
  .in(file("."))
  .settings(
    commonSettings,
    name := "Compile Time LL1 Parser",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    ),
    Compile / doc / scalacOptions ++= {
        Seq("-doc-root-content", (Compile / sourceDirectory).value + "/rootdoc.txt")
    },
  )

lazy val example = Project("example", file("example"))
  .settings(
    commonSettings,
    name := "example",
    Compile / scalaSource  := baseDirectory.value,
  )
  .dependsOn(root)


  lazy val benchmark = Project("benchmark", file("benchmark"))
  .settings(
    commonSettings,
    name := "benchmark",
    Compile / scalaSource  := baseDirectory.value,
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases",
    libraryDependencies += ("com.storm-enroute" %% "scalameter" % "0.21").cross(CrossVersion.for3Use2_13),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    Test / parallelExecution := false,
    fork := true,
    outputStrategy := Some(StdoutOutput),
    connectInput := true
  )
  .dependsOn(root)


