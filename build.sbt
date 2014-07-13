organization  := "com.github.wedens"

name          := "vim-scala-completion"

version       := "0.1"

scalaVersion  := "2.11.1"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature")

resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/"
)

libraryDependencies ++= {
  val akkaV = "2.3.4"
  val sprayV = "1.3.1"
  Seq(
    "org.scala-lang"      %   "scala-compiler" % scalaVersion.value,
    "io.spray"            %%  "spray-can"      % sprayV,
    "io.spray"            %%  "spray-routing"  % sprayV,
    "com.typesafe.akka"   %%  "akka-actor"     % akkaV,
    "org.scalaz"          %%  "scalaz-core"    % "7.0.6",
    "com.typesafe.akka"   %%  "akka-testkit"   % akkaV    % "test",
    "org.specs2"          %%  "specs2"         % "2.3.12" % "test",
    "io.spray"            %%  "spray-testkit"  % sprayV   % "test"
  )
}

seq(Revolver.settings: _*)
