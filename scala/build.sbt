name := "adaptrape"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"


version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "org.typelevel" %% "spire" % "0.16.0"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
