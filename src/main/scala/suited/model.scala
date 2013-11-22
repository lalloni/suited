package suited

import scala.annotation.implicitNotFound

package object model {
  implicit def scalar[T: Supported](value: T) = Scalar(value)
  implicit def scalarField[T: Supported](pair: (String, T)): Field = Field(pair._1, Scalar(pair._2))
  implicit def valueField[V <: Value](pair: (String, V)): Field = Field(pair._1, pair._2)
}

package model {

  sealed trait Value

  case class Field(name: String, value: Value)

  case class Record(fields: Field*) extends Value with UniqueFieldNames {
    def fieldNames = fields.toList.map(_.name)
  }

  case class Sequence(values: Value*) extends Value

  case class Scalar[T: Supported](value: T) extends Value

  @implicitNotFound("The type ${T} is not a supported scalar type")
  sealed class Supported[T] private[model] ()

  object Supported {

    implicit val integerSupport = new Supported[Integer]()
    implicit val intSupport = new Supported[Int]()
    implicit val longSupport = new Supported[Long]()
    implicit val javaLongSupport = new Supported[java.lang.Long]()
    implicit val bigIntSupport = new Supported[BigInt]()

    implicit val floatSupport = new Supported[Float]()
    implicit val doubleSupport = new Supported[Double]()
    implicit val bigDecimalSupport = new Supported[BigDecimal]()

    implicit val stringSupport = new Supported[String]()

    import org.joda.time.{ DateTime, Interval, LocalDate, Period, YearMonth }

    implicit val dateTimeSupport = new Supported[DateTime]()
    implicit val localDateSupport = new Supported[LocalDate]()
    implicit val yearMonthSupport = new Supported[YearMonth]()

    implicit val periodSupport = new Supported[Period]
    implicit val intervalSupport = new Supported[Interval]

  }

}
