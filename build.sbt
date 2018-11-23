name := "ManagementSystem"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

resourceDirectory in Compile := (scalaSource in Compile).value
libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "8.0.102-R11",
  "org.scalafx" %% "scalafxml-core-sfx8" % "0.3"
)

// https://mvnrepository.com/artifact/org.xerial/sqlite-jdbc
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.7.2"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

fork := true

shellPrompt := { state => System.getProperty("user.name") + s":${name.value}> " }