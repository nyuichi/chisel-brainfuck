lazy val root = project.in(file(".")).dependsOn(githubRepo)

lazy val githubRepo = uri("git://github.com/wasabiz/chisel-uart.git")

scalaVersion := "2.11.7"

libraryDependencies += "edu.berkeley.cs" %% "chisel" % "latest.release"
