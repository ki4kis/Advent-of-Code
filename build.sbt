name := "CoA-2021"
version := "0.1"

scalaVersion := "2.13.5"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.5"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.0"
libraryDependencies += "com.softwaremill.magnolia1_2" %% "magnolia" % "1.0.0-M7"