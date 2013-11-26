package suited

import org.scalatest.FunSuite
import model._
import model.dsl._
import types._
import types.dsl._
import types.validators._
import org.scalatest.Matchers

class TypeTest extends FunSuite with Matchers {

  test("ScalarType") {
    ScalarType[String](None).check(Scalar("papa")) should be ('passed)
    ScalarType[String](None).check(Scalar(200)) should be ('failed)
    RecordType(FieldType("name", ScalarType[String](None))).check(record("name" → "pepe")) should be ('passed)
    ("name" is any[String]) ~ ("age" is any[Int]) check (record("name" → "pepe", "age" → 2)) should be ('passed)
    ("name" is (any[String] and isCUIT)) ~ ("age" is (any[Int] and lessThan(100))) check (record("name" → "12345678901", "age" → 2)) should be ('passed)
  }

  val contribuyente = (
    ("cuit" is (any[String] and isCUIT)) ~
    ("tipo" is any[Int]) ~
    ("periodo" is ("año" is any[Int]) ~ ("mes" is any[Long])) ~
    ("impuestos" are (many are any[Int])) ~
    ("blas" are (3 orLess any[String]))
  )

  val recordTypeFull =
    RecordType(
      FieldType("name", ScalarType(Some(Type[String] and nonEmpty))),
      FieldType("age", ScalarType(Some(Type[Int]))),
      FieldType("tags", SequenceType(Unbounded, ScalarType(Some(Type[String])))),
      FieldType("bla", SequenceType(Exactly(1), ScalarType(Some(Type[String])))),
      FieldType("address", RecordType(
        FieldType("street", ScalarType(Some(Type[String]))),
        FieldType("number", ScalarType(Some(Type[Long])))
      ))
    )

  val recordTypeDSL =
    ("name" is any[String]) ~
      ("age" is any[Int]) ~
      ("tags" are (many are any[String])) ~
      ("bla" are (5 are any[Int])) ~
      ("address" is ("street" is any[String]) ~ ("number" is any[Long])) ~
      ("algo" is recordTypeFull + contribuyente)

  def main(ss: Array[String]): Unit = {

    println(contribuyente)

    val simple = ("entero" is any[Int]) ~ ("nombre" is any[String])

    println(simple)

    val rec1 = record("entero" → 6, "nombre" → "pepe")
    println(rec1)
    println(simple.check(rec1))

    val rec2 = record("entero" → 6, "nombre" → 2)
    println(rec2)
    println(simple.check(rec2))

    println(ScalarType(Some(Type[String])).check(Scalar("pepe")))

    println(ScalarType(Some(Type[Int])).check(Scalar("pepe")))

    println(ScalarType(Some(Type[Int])).check(sequence(2, 2)))

    println(RecordType(FieldType("name", ScalarType(Some(Type[String])))).check(record("name" → 2)))

    println(RecordType(FieldType("name", ScalarType(Some(Type[String])))).check(record("name" → "pedro", "age" → 9)))

    println(RecordType(FieldType("name", ScalarType(Some(Type[String]))), FieldType("age", ScalarType(Some(Type[Long])))).check(record("name" → "pedro", "age" → 9l)))

    println(SequenceType(Unbounded, ScalarType(Some(Type[Int]))).check(sequence(2, 2, "a")))

  }

}
