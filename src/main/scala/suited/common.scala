package suited

private[suited] trait UniqueFieldNames {
  private val dupes = fieldNames.groupBy(identity).mapValues(_.size).filter({ case (n, c) ⇒ c > 1 })
  if (dupes.nonEmpty) throw new IllegalArgumentException(s"Duplicate field(s) ${dupes.keys.mkString(", ")} in $this")
  def fieldNames: List[String]
}
