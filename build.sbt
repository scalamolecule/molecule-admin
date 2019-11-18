import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType


lazy val client = (project in file("client"))
  .settings(
    Settings.common,
    Settings.client,
//    Settings.shared,
//    scalaJSUseMainModuleInitializer := true,
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb, TzdbPlugin)
  .dependsOn(sharedJs)


lazy val server = (project in file("server"))
  .settings(
    Settings.common,
    Settings.server,
    scalaJSProjects := Seq(client),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    //    pipelineStages := Seq(digest, gzip),
    // triggers scalaJSPipeline when using compile or continuous compilation
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .dependsOn(sharedJvm)



lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(
    Settings.common,
    Settings.shared
  )
//  .jsSettings(
//    Settings.client,
//  )
//  .jvmSettings(
//    Settings.server,
//  )


lazy val sharedJs  = shared.js
lazy val sharedJvm = shared.jvm

// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen { s: State => "project server" :: s }

