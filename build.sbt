lazy val advent2018 = (project in file("."))
  .settings (
    name := "Advent2018",
    organization := "org.saegesser",
    version := "0.0.0-SNAPSHOT",
    scalaVersion in ThisBuild := "2.12.7",
    scalacOptions in ThisBuild ++= Seq(
      "-feature",
      "-deprecation",
      "-Yno-adapted-args",
      "-Ywarn-value-discard",
      "-Ywarn-numeric-widen",
      "-Ywarn-dead-code",
      "-Xlint",
      "-Xfatal-warnings",
      "-unchecked",
      "-language:implicitConversions"
    ),

    scalacOptions in (Compile, console) ~= (_.filterNot(_ == "-Xlint")),
    scalacOptions in (Test, console) ~= (_.filterNot(_ == "-Xlint")),

    libraryDependencies ++= Seq(
      "org.scalatest"              %% "scalatest"                      % "3.0.0"   % "test",
      "org.scalacheck"             %% "scalacheck"                     % "1.13.4"  % "test"
    ),

    initialCommands := "import advent._"
  )
