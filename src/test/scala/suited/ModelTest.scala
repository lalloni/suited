package suited

import org.joda.time.{ DateTime, Interval, LocalDate, YearMonth }

import com.github.nscala_time.time.Imports.richInt

import model._

object ModelTest {

  val data =
    Record(
      "nombre" → "Pablo",
      "edad" → 99.0,
      "masa" → BigDecimal(99.0),
      "otros" → Sequence(10, 1, Record(
        "a" → 10,
        "b" → 11)),
      "bla" → DateTime.now,
      "periodo" → new YearMonth(2000, 10),
      "direccion" →
        Record(
          "calle" → "blabla",
          "numero" → 100,
          "desde" → LocalDate.now,
          "período" → 10.days),
      "vigencia" → LocalDate.now.minus(6.months).toInterval.withPeriodAfterStart(60.days),
      "intervalo" → Interval.parse("201001/P1M"))

  def main(a: Array[String]): Unit =
    println(data)

}
