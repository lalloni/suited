resolvers += "Arquitectura Releases" at "http://artifactsddit.afip.gob.ar/nexus/content/repositories/arquitectura"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "0.6.0"

libraryDependencies ++= Seq("joda-time" % "joda-time" % "2.3", "org.joda" % "joda-convert" % "1.5")

libraryDependencies += "org.spire-math" %% "spire" % "0.6.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"
