package suited

case class DuplicateFieldException(message: String) extends Exception(message)

private[suited] trait UniqueFieldNames {
  private val dupes = fieldNames.groupBy(identity).mapValues(_.size).filter({ case (n, c) ⇒ c > 1 })
  if (dupes.nonEmpty) throw DuplicateFieldException(s"Duplicate field(s) ${dupes.keys.mkString(", ")} in $this")
  def fieldNames: List[String]
}
