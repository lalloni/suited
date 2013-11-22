package suited

import org.scalatest.FunSuite
import model._
import types._
import types.dsl._
import types.validators._
import org.scalatest.Matchers
import org.scalautils._

class TypeTest extends FunSuite with Matchers {

  val good = a[Good[Ok, Error]]
  val bad = a[Bad[Ok, Error]]

  test("ScalarType") {
    ScalarType(any[String]).apply(Scalar("papa")) should be (good)
    ScalarType(any[String]).apply(Scalar(200)) should be (bad)
    RecordType(FieldType("name", any[String])).apply(Record("name" → "pepe")) should be (good)
    ("name" is any[String]) ~ ("age" is any[Integer]) apply (Record("name" → "pepe", "age" → 2)) should be (good)
    ("name" is (any[String] and isCUIT and hasLength(lessThan(5)))) ~ ("age" is (any[Int] and lessThan(100))) apply (Record("name" → "123456", "age" → 2)) should be (good)
  }

  val contribuyente = (
    ("cuit" is (any[String] and isCUIT)) ~
    ("tipo" is any[Int]) ~
    ("periodo" is ("año" is any[Int]) ~ ("mes" is any[Long])) ~
    ("impuestos" are (one is any[String]) & (many are any[Int]) & (many are ("a" is any[String]))) ~
    ("blas" are
      (2 are any[Long]) &
      (3 orLess any[String]) &
      (2 orMore any[Int]) &
      (many are ("name" is any[BigDecimal])) &
      (5 are ("id" is any[Int]) ~ ("name" is any[String])) &
      (one is
        ("id" is any[Int]) ~
        ("name" is any[String])
      )
    )
  )

  val recordTypeFull =
    RecordType(
      FieldType("name", ScalarType(any[String] and nonEmpty)),
      FieldType("age", ScalarType(any[Int])),
      FieldType("tags", SequenceType(Segment(Unbounded, ScalarType(any[String])))),
      FieldType("bla", SequenceType(Segment(Exactly(1), ScalarType(any[String])), Segment(Exactly(1), ScalarType(any[Int])))),
      FieldType("address", RecordType(
        FieldType("street", ScalarType(any[String])),
        FieldType("number", ScalarType(any[Long]))
      ))
    )

  val recordTypeDSL =
    ("name" is any[String]) ~
      ("age" is any[Int]) ~
      ("tags" are (many are any[String])) ~
      ("bla" are (one is any[String]) & (5 are any[Int])) ~
      ("address" is ("street" is any[String]) ~ ("number" is any[Long])) ~
      ("algo" is recordTypeFull + contribuyente)

  def main(ss: Array[String]): Unit = {

    println(contribuyente)

    val simple = ("entero" is any[Int]) ~ ("nombre" is any[String])

    println(simple)

    val rec1 = Record("entero" → 6, "nombre" → "pepe")
    println(rec1)
    println(simple.apply(rec1))

    val rec2 = Record("entero" → 6, "nombre" → 2)
    println(rec2)
    println(simple.apply(rec2))

    println(ScalarType(any[String]).apply(Scalar("pepe")))

    println(ScalarType(any[Int]).apply(Scalar("pepe")))

    println(ScalarType(any[Int]).apply(Sequence(2, 2)))

    println(RecordType(FieldType("name", ScalarType(any[String]))).apply(Record("name" → 2)))

    println(RecordType(FieldType("name", ScalarType(any[String]))).apply(Record("name" → "pedro", "age" → 9)))

    println(RecordType(FieldType("name", ScalarType(any[String])), FieldType("age", ScalarType(any[Long]))).apply(Record("name" → "pedro", "age" → 9l)))

    println(SequenceType(Segment(Unbounded, ScalarType(any[Int]))).apply(Sequence(2, 2, "a")))

  }

}
