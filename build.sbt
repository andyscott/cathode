lazy val akkaVersion          = "2.4.9"
lazy val catsVersion          = "0.7.2"
lazy val monixVersion         = "2.0.0"

lazy val scalatestVersion     = "3.0.0"
lazy val scalacheckVersion    = "1.13.2"
lazy val disciplineVersion    = "0.4"
lazy val catsScalatestVersion = "1.4.0"

lazy val root = (project in file("."))
  .aggregate(
  `cathode-core`,
  `cathode-monix`)

lazy val `cathode-core` = (project in file("cathode-core"))
  .settings(name := "cathode-core")
  .settings(libraryDependencies ++= Seq(
    "org.typelevel"     %% "cats-core"      % catsVersion,
    "org.typelevel"     %% "cats-free"      % catsVersion,
    "com.typesafe.akka" %% "akka-actor"     % akkaVersion
  ))
  .settings(libraryDependencies ++= Seq(
    "org.scalacheck"    %% "scalacheck"     % scalacheckVersion,
    "org.scalatest"     %% "scalatest"      % scalatestVersion,
    "com.typesafe.akka" %% "akka-testkit"   % akkaVersion
  ).map(_ % "test"))

lazy val `cathode-monix` = (project in file("cathode-monix"))
  .settings(name := "cathode-monix")
  .dependsOn(`cathode-core`)
  .settings(libraryDependencies ++= Seq(
    "io.monix"          %% "monix-eval"     % monixVersion
  ))
