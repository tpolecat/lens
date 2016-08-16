scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-encoding", "UTF-8", // 2 args
  "-feature",                
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",       
  "-Ywarn-dead-code",       
  "-Ywarn-value-discard"
)

// libraryDependencies ++= Seq(
//   "org.tpolecat" %% "doobie-core"               % "0.2.4",
//   "org.tpolecat" %% "doobie-contrib-h2"         % "0.2.4",
//   "org.tpolecat" %% "atto-core"                 % "0.4.2"
// )

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

tutSettings

tutSourceDirectory := baseDirectory.value / "tut"

tutTargetDirectory := baseDirectory.value / "tut-out"

// addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)
