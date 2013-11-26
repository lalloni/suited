package suited

import org.joda.time.{ DateTime, Interval, LocalDate, YearMonth }
import com.github.nscala_time.time.Imports._
import org.scalatest.FunSuite
import org.scalatest.Matchers

import suited.model._
import suited.model.dsl._

class ModelTest extends FunSuite with Matchers {

  test("Build implicit scalars from supported types") {
    def build[T](scalar: Scalar[T]) = scalar
    build("pepe") should equal (Scalar("pepe"))
  }

  test("Build record") {
    record(
      "nombre" → "Pablo",
      "edad" → 99.0,
      "masa" → BigDecimal(99.0),
      "otros" → sequence(10, 1),
      "bla" → DateTime.now,
      "periodo" → new YearMonth(2000, 10),
      "direccion" →
        record(
          "calle" → "blabla",
          "numero" → 100,
          "desde" → LocalDate.now,
          "período" → 10.days),
      "vigencia" → LocalDate.now.minus(6.months).toInterval.withPeriodAfterStart(60.days),
      "intervalo" → Interval.parse("201001/P1M"))
  }

}
