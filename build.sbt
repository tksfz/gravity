enablePlugins(ScalaJSPlugin)

name := "test"

scalaVersion := "2.11.8"

val Scala211 = "2.11.8"

val scalajsReactVersion = "0.11.1"
val scalaCSSVersion = "0.4.1"

//type PE = Project => Project

def commonSettings: Project => Project =
  _.enablePlugins(ScalaJSPlugin)
    .settings(
      scalaVersion         := Scala211,
      scalacOptions       ++= Seq("-deprecation", "-unchecked", "-feature",
        "-language:postfixOps", "-language:implicitConversions",
        "-language:higherKinds", "-language:existentials"), //"-Ymacro-debug-lite"
      updateOptions        := updateOptions.value.withCachedResolution(true),
      dependencyOverrides ++= Set(
        "org.scala-js"   %% "scalajs-test-interface" % "0.6.11"
      )
    )

val jsDir = "demo/assets"

def createLauncher(scope: String = "compile"): Project => Project =
  _.settings(persistLauncher := true,
    persistLauncher in Test := false,
    crossTarget in (Compile, fullOptJS) := file(jsDir),
    crossTarget in (Compile, fastOptJS) := file(jsDir),
    //      crossTarget in (Compile, packageLauncher) := file(jsDir),
    artifactPath in (Compile, fastOptJS) := ((crossTarget in (Compile, fastOptJS)).value /
      ((moduleName in fastOptJS).value + "-opt.js"))
  )

lazy val demo = project
    .settings(
      libraryDependencies ++= Seq(
        "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1",
        "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.1",
        "com.github.chandu0101.scalajs-react-components" %%% "core" % "0.5.0",
        "com.github.japgolly.scalacss"      %%% "core"     % scalaCSSVersion,
        "com.github.japgolly.scalacss"      %%% "ext-react" % scalaCSSVersion
      )
    )
  .configure(commonSettings, createLauncher())