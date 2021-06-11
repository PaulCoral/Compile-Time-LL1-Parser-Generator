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


  lazy val benchmark = Project("benchmark", file("benchmark"))
  .settings(
    commonSettings,
    name := "benchmark",
    scalaSource in Compile := baseDirectory.value,
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases",
    libraryDependencies += ("com.storm-enroute" %% "scalameter" % "0.21").cross(CrossVersion.for3Use2_13),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false,
    fork := true,
    outputStrategy := Some(StdoutOutput),
    connectInput := true
  )
  .dependsOn(root)