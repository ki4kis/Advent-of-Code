name := "advent-of-code"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.10",
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % "2.13.10",
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.typelevel" %% "cats-effect" % "3.4.2",
    "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.2"
  )
)

lazy val common = project
  .settings(commonSettings)

lazy val `year-2020` = (project in file("2020"))
  .dependsOn(common)
  .settings(commonSettings)

lazy val `year-2021` = (project in file("2021"))
  .dependsOn(common)
  .settings(commonSettings)

lazy val `year-2022` = (project in file("2022"))
  .dependsOn(common)
  .settings(commonSettings)

// libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.5"
// libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
// libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0"
// libraryDependencies += "com.softwaremill.magnolia1_2" %% "magnolia" % "1.0.0-M7"
