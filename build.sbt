name := "YASCC"

version := "0.1"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-language:postfixOps", "-unchecked", "-deprecation", "-feature")

scalaSource in Compile <<= baseDirectory(_ / "src")

//scalaSource in Compile <<= (sourceDirectory in Compile)(_ / "src")
//libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.6.1"

libraryDependencies ++= List(
  "com.github.scopt" %% "scopt" % "3.1.0",
  "org.scalaz" %% "scalaz-core" % "7.0.2" withSources(),
  "com.googlecode.kiama" %% "kiama" % "1.5.1" withSources()
)

//resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"
