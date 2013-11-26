resolvers += "Arquitectura Releases" at "http://artifactsddit.afip.gob.ar/nexus/content/repositories/arquitectura"

libraryDependencies += "com.chuusai" % "shapeless" % "2.0.0-M1" cross CrossVersion.full

//libraryDependencies ++= Seq("scalaz-core", "scalaz-iteratee") map ("org.scalaz" %% _ % "7.0.3")

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "0.6.0"

libraryDependencies ++= Seq("joda-time" % "joda-time" % "2.3", "org.joda" % "joda-convert" % "1.5")

libraryDependencies += "org.spire-math" %% "spire" % "0.6.0"

libraryDependencies += "org.scalautils" %% "scalautils" % "2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"
