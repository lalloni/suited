publishTo := {
  val nexus = "http://artifactsddit.afip.gob.ar/nexus/"
  if (isSnapshot.value)
    Some("arquitectura snapshots" at nexus + "content/repositories/arquitectura-snapshots")
  else
    Some("arquitectura releases"  at nexus + "content/repositories/arquitectura")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".dit-nexus-credentials")
