lazy val advent2018 = (project in file("."))
  .settings (
    name := "Advent2017",
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
      "-language:implicitConversions",
      "-language:higherKinds"
    ),

    scalacOptions in (Compile, console) ~= (_.filterNot(_ == "-Xlint")),
    scalacOptions in (Test, console) ~= (_.filterNot(_ == "-Xlint")),

    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7"),

    libraryDependencies ++= Seq(
      "org.scalaz"       %% "scalaz-core"    % "7.3.0-M26",
      "org.scalaz"       %% "scalaz-effect"  % "7.3.0-M26",
      "org.scalatest"    %% "scalatest"      % "3.0.0"            % "test",
      "org.scalacheck"   %% "scalacheck"     % "1.13.4"           % "test"
    ),

    initialCommands := "import advent._"
  )
