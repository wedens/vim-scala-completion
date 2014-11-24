organization  := "com.github.wedens"

name          := "vim-scala-completion"

version       := "0.1"

scalaVersion  := "2.11.4"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/"
)

libraryDependencies ++= {
  val akkaV = "2.3.7"
  val sprayV = "1.3.2"
  Seq(
    "org.scala-lang"      %   "scala-compiler"  % scalaVersion.value,
    // "org.scalaz"          %%  "scalaz-core"     % "7.0.6",
    "ch.qos.logback"      %   "logback-classic" % "1.1.1",
    "io.spray"            %%  "spray-can"       % sprayV,
    "io.spray"            %%  "spray-routing"   % sprayV,
    "io.spray"            %%  "spray-testkit"   % sprayV   % "test",
    "com.typesafe.akka"   %%  "akka-actor"      % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"    % akkaV    % "test",
    "com.typesafe.akka"   %%  "akka-slf4j"      % akkaV,
    "org.specs2"          %%  "specs2"          % "2.3.12" % "test"
  )
}

seq(Revolver.settings: _*)
