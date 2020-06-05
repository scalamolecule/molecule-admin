import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType


lazy val client = (project in file("client"))
  .dependsOn(sharedJS)
  .settings(Settings.client)
  .enablePlugins(ScalaJSWeb)
//  .enablePlugins(ScalaJSWeb, TzdbPlugin)


lazy val server = (project in file("server"))
  .dependsOn(sharedJVM)
  .settings(
    Settings.server,
    scalaJSProjects := Seq(client),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    pipelineStages := Seq(rjs, digest, gzip),

    // triggers scalaJSPipeline when using compile or continuous compilation
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
//  .enablePlugins(MoleculePlugin).settings(moleculeSchemas := Seq("db/admin", "db/core", "db/integration", "db/migration"))


lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(Settings.shared)

lazy val sharedJS  = shared.js
lazy val sharedJVM = shared.jvm

// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen { s: State => "project server" :: s }
