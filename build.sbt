val scala3Version = "3.1.1"

val commonSettings = Seq(
  version            := "0.1.0",
  scalaVersion       := scala3Version,
)

lazy val root = project.in(file(".")).
  aggregate(foo.js, foo.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )


lazy val foo = crossProject(JSPlatform,JVMPlatform)
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
  ).jsSettings(
      scalaJSUseMainModuleInitializer := true,
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
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
    fork := true,
    run / baseDirectory := file("."),
    //javaOptions in run     += "-Xss1024K",
    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "src",
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases",
    libraryDependencies += ("com.storm-enroute" %% "scalameter" % "0.21").cross(CrossVersion.for3Use2_13),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    Test / parallelExecution := false,
    connectInput := true,
    logBuffered := false,

  )
  .dependsOn(root)


