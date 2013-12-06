package suited

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import scala.reflect.runtime.universe._

package model {

  sealed trait Value {
    def description: String
  }

  case class Scalar[S: Support: TypeTag](value: S) extends Value {
    def description = s"""Scalar of ${typeTag[S].tpe} "$value""""
  }

  case class Sequence(values: List[Value]) extends Value {
    def description = s"Sequence of ${values.map(_.description).mkString(", ")}"
  }

  case class Field(name: String, value: Value) {
    def description = s"$name is ${value.description}"
  }

  case class Record(fields: List[Field]) extends Value with UniqueFieldNames {
    def fieldNames = fields.toList.map(_.name)
    def description = s"Record of ${fields.map(_.description).mkString(", ")}"
  }

  @implicitNotFound("${S} is not a supported scalar type")
  sealed case class Support[S] private[model] ()

  object Support {

    implicit val string = new Support[String]

    implicit val integer = new Support[Integer]
    implicit val int = new Support[Int]
    implicit val long = new Support[Long]
    implicit val bigInt = new Support[BigInt]

    implicit val float = new Support[Float]
    implicit val double = new Support[Double]
    implicit val bigDecimal = new Support[BigDecimal]

    import spire.math.{ Natural, Rational, Real, SafeLong, Number }

    implicit val natural = new Support[Natural]
    implicit val safeLong = new Support[SafeLong]

    implicit val rational = new Support[Rational]
    implicit val real = new Support[Real]

    implicit val number = new Support[Number]

    import org.joda.time.{ DateTime, Interval, LocalDate, Period, YearMonth }

    implicit val dateTime = new Support[DateTime]
    implicit val localDate = new Support[LocalDate]
    implicit val yearMonth = new Support[YearMonth]

    implicit val period = new Support[Period]
    implicit val interval = new Support[Interval]

  }

  object dsl {

    implicit def scalar[S: Support: TypeTag](value: S): Scalar[S] =
      Scalar(value)

    implicit def field[S: Support: TypeTag](pair: (String, S)): Field =
      Field(pair._1, Scalar(pair._2))

    implicit def field(pair: (String, Value)): Field =
      Field(pair._1, pair._2)

    def record(fields: Field*): Record =
      Record(fields.toList)

    def sequence(values: Value*): Sequence =
      Sequence(values.toList)

    implicit class RecordOps(self: Record) {
      def ++(other: Record): Record =
        Record(self.fields ++ other.fields)
      def ++(fields: Field*): Record =
        Record(self.fields ++ fields)
    }

    implicit class SequenceOps(self: Sequence) {
      def ++(other: Sequence): Sequence =
        Sequence(self.values ++ other.values)
      def ++(values: Value*): Sequence =
        Sequence(self.values ++ values)
    }

  }

}
