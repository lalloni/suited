package suited

import model._
import types._
import dsl._
import validators._

object TypeTest {

  val contribuyente = (
    ("cuit" is of[String](isCUIT)) ~
    ("tipo" is of[Int]) ~
    ("periodo" is ("año" is of[Int]) ~ ("mes" is of[Long])) ~
    ("impuestos" are (one is of[String]) & (many are of[Int]) & (many are ("a" is of[String]))) ~
    ("blas" are
      (2 are of[Long]) &
      (3 orLess of[String]) &
      (2 orMore of[Int]) &
      (many are ("name" is of[BigDecimal])) &
      (5 are ("id" is of[Int]) ~ ("name" is of[String])) &
      (one is
        ("id" is of[Int]) ~
        ("name" is of[String])
      )
    )
  )

  val recordTypeFull =
    RecordType(
      FieldType("name", ScalarType(str(nonEmpty))),
      FieldType("age", ScalarType(of[Int])),
      FieldType("tags", SequenceType(Segment(Unbounded, ScalarType(of[String])))),
      FieldType("bla", SequenceType(Segment(Exactly(1), ScalarType(of[String])), Segment(Exactly(1), ScalarType(of[Int])))),
      FieldType("address", RecordType(
        FieldType("street", ScalarType(of[String])),
        FieldType("number", ScalarType(of[Long]))
      ))
    )

  val recordTypeDSL =
    ("name" is of[String]) ~
      ("age" is of[Int]) ~
      ("tags" are (many are of[String])) ~
      ("bla" are (one is of[String]) & (5 are of[Int])) ~
      ("address" is ("street" is of[String]) ~ ("number" is of[Long])) ~
      ("algo" is recordTypeFull + contribuyente)

  def main(ss: Array[String]): Unit = {

    println(contribuyente)

    val simple = ("entero" is of[Int]) ~ ("nombre" is of[String])

    println(simple)

    val rec1 = Record("entero" → 6, "nombre" → "pepe")
    println(rec1)
    println(simple.validates(rec1))

    val rec2 = Record("entero" → 6, "nombre" → 2)
    println(rec2)
    println(simple.validates(rec2))

    println(ScalarType(of[String]).validates(Scalar("pepe")))

    println(ScalarType(of[Int]).validates(Scalar("pepe")))

    println(ScalarType(of[Int]).validates(Sequence(2, 2)))

    println(RecordType(FieldType("name", ScalarType(of[String]))).validates(Record("name" → 2)))

    println(RecordType(FieldType("name", ScalarType(of[String]))).validates(Record("name" → "pedro", "age" → 9)))

    println(RecordType(FieldType("name", ScalarType(of[String])), FieldType("age", ScalarType(of[Long]))).validates(Record("name" → "pedro", "age" → 9l)))

    println(SequenceType(Segment(Unbounded, ScalarType(of[Int]))).validates(Sequence(2, 2, "a")))

  }

}
