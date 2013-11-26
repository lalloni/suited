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
    anyScalar[String] check scalar("papa") should be ('passed)
    anyScalar[Int] check scalar(100) should be ('passed)
    anyScalar[String] check Scalar(200) should be ('failed)
    anyScalar[String](hasLength(lessThan(5))) check Scalar(200) should be ('failed)
    anyScalar[String](hasLength(lessThan(5))) check Scalar("12345") should be ('failed)
    anyScalar[String](hasLength(lessThan(5))) check Scalar("1234") should be ('passed)
  }

  test("SequenceType") {
    anySequence(many of any[Int]) check sequence(1, 2, 3) should be ('passed)
    anySequence(many of any[Int]) check sequence() should be ('passed)
    anySequence(2 orMore any[Int]) check sequence(1, 2, 3) should be ('passed)
    anySequence(2 orMore any[Int]) check sequence(1, 2) should be ('passed)
    anySequence(2 orMore any[Int]) check sequence(1) should be ('failed)
    anySequence(2 orLess any[Int]) check sequence(1, 2, 3) should be ('failed)
    anySequence(2 orLess any[Int]) check sequence(1, 2) should be ('passed)
    anySequence(2 orLess any[Int]) check sequence(1) should be ('passed)
    anySequence(2 of any[Int]) check sequence(1, 2, 3) should be ('failed)
    anySequence(2 of any[Int]) check sequence(1, 2) should be ('passed)
    anySequence(2 of any[Int]) check sequence(1) should be ('failed)
  }

  test("RecordType") {
    ("a" is any[Int]) ~ ("b" is any[Int]) check record("a" → 1, "b" → 2) should be ('passed)
    ("a" is any[Int]) ~ ("b" is any[Int]) check record("a" → 1) should be ('failed)
    ("a" is any[Int]) ~ ("b" is any[Int]) check record("a" → 1, "b" → 2, "c" → 3) should be ('passed)
  }

  test("FusionType") {
    (("a" is any[Int]) ~ ("b" is any[Int])) + (("c" is any[String]) ~ ("d" is any[String])) check record("a" → 1, "b" → 2, "c" → "3", "d" → "4") should be ('passed)
    (("a" is any[Int]) ~ ("b" is any[Int])) + (("c" is any[String]) ~ ("d" is any[String])) check record("a" → 1, "b" → 2, "c" → "3", "d" → "4", "e" → 5) should be ('passed)
    (("a" is any[Int]) ~ ("b" is any[Int])) + (("c" is any[String]) ~ ("d" is any[String])) check record("a" → 1, "b" → 2, "c" → "3", "d" → 4) should be ('failed)
    (("a" is any[Int]) ~ ("b" is any[Int])) + (("c" is any[String]) ~ ("d" is any[String])) check record("a" → 1, "b" → 2, "c" → "3") should be ('failed)
    a[DuplicateFieldException] should be thrownBy (("a" is any[Int]) ~ ("b" is any[Int])) + (("a" is any[String]) ~ ("d" is any[String]))
  }

}
