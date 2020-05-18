import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossType


lazy val client = (project in file("client"))
  .dependsOn(sharedJs)
  .settings(
    Settings.client,
//    jsDependencies += RuntimeDOM,
//    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )
  .enablePlugins(ScalaJSWeb, TzdbPlugin)


lazy val server = (project in file("server"))
  .dependsOn(sharedJvm)
  .settings(
    Settings.server,
    scalaJSProjects := Seq(client),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    pipelineStages := Seq(rjs, digest, gzip),

    // triggers scalaJSPipeline when using compile or continuous compilation
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  )
  .enablePlugins(PlayScala, JavaAgent)
  .disablePlugins(PlayLayoutPlugin)
//  .enablePlugins(MoleculePlugin).settings(moleculeSchemas := Seq("db/admin", "db/core", "db/integration", "db/migration"))


lazy val shared = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(Settings.shared)

lazy val sharedJs  = shared.js
lazy val sharedJvm = shared.jvm

// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen { s: State => "project server" :: s }

