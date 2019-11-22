name := "sjsd3x"

version := "0.1"

scalaVersion := "2.12.8"
useYarn := true

lazy val d3testPAGE = (project in file("d3TestPage"))
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
  .dependsOn(core)
  .settings(
    // scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7",
    libraryDependencies += "com.github.fdietze" %%% "scala-js-d3v4" % "master-SNAPSHOT",
    scalaJSUseMainModuleInitializer := true,
  )

lazy val core = (project in file("core"))
  .enablePlugins(ScalaJSPlugin)
  .settings()
