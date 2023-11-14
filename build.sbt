ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.13.8"
ThisBuild / organization := "de.hpi.osm"

Compile / scalaSource := baseDirectory.value / "src"
Compile / resourceDirectory := baseDirectory.value / "resources"


val spinalVersion = "1.9.4"
libraryDependencies += "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
libraryDependencies += "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
libraryDependencies += compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

libraryDependencies += "me.vican.jorge" %% "dijon" % "0.6.0"
libraryDependencies += "org.jline" % "jline-terminal" % "3.11.0"

run / baseDirectory := baseDirectory.value / "build"

run / connectInput := true
run / outputStrategy := Some(StdoutOutput)

fork := true
