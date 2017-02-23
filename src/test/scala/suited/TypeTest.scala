package suited

import org.scalatest.FunSuite
import model._
import model.dsl._
import types._
import types.dsl._
import types.validators._
import org.scalatest.Matchers
import org.joda.time.LocalDate

class TypeTest extends FunSuite with Matchers {

  test("CES") {
    CES check "AAAAAA0000" should be ('passed)
    CES check "AAAAAAZ000" should be ('failed)
    CES check "AAA2000000" should be ('passed)
  }

  test("AnyValueType") {
    anyValue check 1 should be ('passed)
    anyValue check "a" should be ('passed)
    anyValue check record("x" → 1) should be ('passed)
    anyValue check sequence(1, "a", LocalDate.now) should be ('passed)
  }

  test("ScalarType") {
    anyScalar[String] check scalar("papa") should be ('passed)
    anyScalar[Int] check scalar(100) should be ('passed)
    anyScalar[String] check Scalar(200) should be ('failed)
    anyScalar[String](hasLength(lesserThan(5))) check Scalar(200) should be ('failed)
    anyScalar[String](hasLength(lesserThan(5))) check Scalar("12345") should be ('failed)
    anyScalar[String](hasLength(lesserThan(5))) check Scalar("1234") should be ('passed)
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
    anySequence(many of anyValue) check sequence(1, "a", 2.0) should be ('passed)
  }

  test("RecordType") {
    ("a" is any[Int]) ~ ("b" is any[Int]) check record("a" → 1, "b" → 2) should be ('passed)
    ("a" is any[Int]) ~ ("b" is any[Int]) check record("a" → 1) should be ('failed)
    ("a" is any[Int]) ~ ("b" is any[Int]) check record("a" → 1, "b" → 2, "c" → 3) should be ('passed)
    anyRecord check record() should be ('passed)
    anyRecord check record("a" → 1) should be ('passed)
    anyRecord check 1 should be ('failed)
    anyRecord check sequence(1, 2, 3) should be ('failed)
  }

  test("FusionType") {
    (("a" is any[Int]) ~ ("b" is any[Int])) + (("c" is any[String]) ~ ("d" is any[String])) check record("a" → 1, "b" → 2, "c" → "3", "d" → "4") should be ('passed)
    (("a" is any[Int]) ~ ("b" is any[Int])) + (("c" is any[String]) ~ ("d" is any[String])) check record("a" → 1, "b" → 2, "c" → "3", "d" → "4", "e" → 5) should be ('passed)
    (("a" is any[Int]) ~ ("b" is any[Int])) + (("c" is any[String]) ~ ("d" is any[String])) check record("a" → 1, "b" → 2, "c" → "3", "d" → 4) should be ('failed)
    (("a" is any[Int]) ~ ("b" is any[Int])) + (("c" is any[String]) ~ ("d" is any[String])) check record("a" → 1, "b" → 2, "c" → "3") should be ('failed)
    a[DuplicateFieldException] should be thrownBy (("a" is any[Int]) ~ ("b" is any[Int])) + (("a" is any[String]) ~ ("d" is any[String]))
  }

}
