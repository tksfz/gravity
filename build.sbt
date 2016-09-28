enablePlugins(ScalaJSPlugin)

name := "test"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1",
  "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.1",
  "com.github.chandu0101.scalajs-react-components" %%% "core" % "0.5.0"
)
