lazy val root = project.in(file(".")).dependsOn(githubRepo)

lazy val githubRepo = uri("git://github.com/wasabiz/chisel-uart.git")

scalaVersion := "2.10.4"

libraryDependencies += "edu.berkeley.cs" %% "chisel" % "latest.release"
