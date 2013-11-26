package suited

import org.scalatest._

import suited.model._
import suited.model.dsl._
import suited.types.dsl._
import suited.types.validators._

class Examples extends FunSuite with Matchers {

  test("Examples") {

    val tipo = (
      ("nombre" is nonEmpty) ~
      ("cuit" is CUIT) ~
      ("ojos" are (2 orLess any[String])) ~
      ("domicilio" is
        ("dirección" is (any[String] and hasLength(greaterThan(5)))) ~
        ("cp" is (any[Int] and greaterThan(999) and lesserThan(10000))) ~
        ("teléfonos" are (many of (any[String] and nonEmpty and matches("\\+.*"))))
      )
    )

    println(tipo) // Imprime: RecordType(WrappedArray(FieldType(nombre,ScalarType(Some(Condition(Non empty)))), FieldType(cuit,ScalarType(Some(Condition(CUIT format)))), FieldType(ojos,SequenceType(EqualOrLess(2),ScalarType(Some(Type(TypeTag[String]))))), FieldType(domicilio,RecordType(WrappedArray(FieldType(dirección,ScalarType(Some(And(Type(TypeTag[String]),View(Length,Condition(More than 5)))))), FieldType(cp,ScalarType(Some(And(And(Type(TypeTag[Int]),Condition(More than 999)),Condition(Less than 10000))))), FieldType(teléfonos,SequenceType(Unbounded,ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/)))))))))))

    val caso1 = record(
      "nombre" → "Pedro López",
      "cuit" → "12345678901",
      "ojos" → sequence("verde", "azul"),
      "domicilio" → record(
        "dirección" → "La Pampa 1234",
        "cp" → 1234,
        "teléfonos" → sequence("+541143473177", "+5491112345678")
      )
    )

    println(caso1) // Imprime: Record(List(Field(nombre,Scalar(Pedro López)), Field(cuit,Scalar(12345678901)), Field(ojos,Sequence(List(Scalar(verde), Scalar(azul)))), Field(domicilio,Record(List(Field(dirección,Scalar(La Pampa 1234)), Field(cp,Scalar(1234)), Field(teléfonos,Sequence(List(Scalar(+541143473177), Scalar(+5491112345678)))))))))

    val result1 = tipo check caso1

    println(result1) // Imprime: RecordReport(RecordType(WrappedArray(FieldType(nombre,ScalarType(Some(Condition(Non empty)))), FieldType(cuit,ScalarType(Some(Condition(CUIT format)))), FieldType(ojos,SequenceType(EqualOrLess(2),ScalarType(Some(Type(TypeTag[String]))))), FieldType(domicilio,RecordType(WrappedArray(FieldType(dirección,ScalarType(Some(And(Type(TypeTag[String]),View(Length,Condition(More than 5)))))), FieldType(cp,ScalarType(Some(And(And(Type(TypeTag[Int]),Condition(More than 999)),Condition(Less than 10000))))), FieldType(teléfonos,SequenceType(Unbounded,ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/))))))))))),Some(Record(List(Field(nombre,Scalar(Pedro López)), Field(cuit,Scalar(12345678901)), Field(ojos,Sequence(List(Scalar(verde), Scalar(azul)))), Field(domicilio,Record(List(Field(dirección,Scalar(La Pampa 1234)), Field(cp,Scalar(1234)), Field(teléfonos,Sequence(List(Scalar(+541143473177), Scalar(+5491112345678)))))))))),List(),List(FieldReport(FieldType(nombre,ScalarType(Some(Condition(Non empty)))),Some(Field(nombre,Scalar(Pedro López))),List(),Some(ScalarReport(ScalarType(Some(Condition(Non empty))),Some(Scalar(Pedro López)),List()))), FieldReport(FieldType(cuit,ScalarType(Some(Condition(CUIT format)))),Some(Field(cuit,Scalar(12345678901))),List(),Some(ScalarReport(ScalarType(Some(Condition(CUIT format))),Some(Scalar(12345678901)),List()))), FieldReport(FieldType(ojos,SequenceType(EqualOrLess(2),ScalarType(Some(Type(TypeTag[String]))))),Some(Field(ojos,Sequence(List(Scalar(verde), Scalar(azul))))),List(),Some(SequenceReport(SequenceType(EqualOrLess(2),ScalarType(Some(Type(TypeTag[String])))),Some(Sequence(List(Scalar(verde), Scalar(azul)))),List(),List(ScalarReport(ScalarType(Some(Type(TypeTag[String]))),Some(Scalar(verde)),List()), ScalarReport(ScalarType(Some(Type(TypeTag[String]))),Some(Scalar(azul)),List()))))), FieldReport(FieldType(domicilio,RecordType(WrappedArray(FieldType(dirección,ScalarType(Some(And(Type(TypeTag[String]),View(Length,Condition(More than 5)))))), FieldType(cp,ScalarType(Some(And(And(Type(TypeTag[Int]),Condition(More than 999)),Condition(Less than 10000))))), FieldType(teléfonos,SequenceType(Unbounded,ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/))))))))),Some(Field(domicilio,Record(List(Field(dirección,Scalar(La Pampa 1234)), Field(cp,Scalar(1234)), Field(teléfonos,Sequence(List(Scalar(+541143473177), Scalar(+5491112345678)))))))),List(),Some(RecordReport(RecordType(WrappedArray(FieldType(dirección,ScalarType(Some(And(Type(TypeTag[String]),View(Length,Condition(More than 5)))))), FieldType(cp,ScalarType(Some(And(And(Type(TypeTag[Int]),Condition(More than 999)),Condition(Less than 10000))))), FieldType(teléfonos,SequenceType(Unbounded,ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/)))))))),Some(Record(List(Field(dirección,Scalar(La Pampa 1234)), Field(cp,Scalar(1234)), Field(teléfonos,Sequence(List(Scalar(+541143473177), Scalar(+5491112345678))))))),List(),List(FieldReport(FieldType(dirección,ScalarType(Some(And(Type(TypeTag[String]),View(Length,Condition(More than 5)))))),Some(Field(dirección,Scalar(La Pampa 1234))),List(),Some(ScalarReport(ScalarType(Some(And(Type(TypeTag[String]),View(Length,Condition(More than 5))))),Some(Scalar(La Pampa 1234)),List()))), FieldReport(FieldType(cp,ScalarType(Some(And(And(Type(TypeTag[Int]),Condition(More than 999)),Condition(Less than 10000))))),Some(Field(cp,Scalar(1234))),List(),Some(ScalarReport(ScalarType(Some(And(And(Type(TypeTag[Int]),Condition(More than 999)),Condition(Less than 10000)))),Some(Scalar(1234)),List()))), FieldReport(FieldType(teléfonos,SequenceType(Unbounded,ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/)))))),Some(Field(teléfonos,Sequence(List(Scalar(+541143473177), Scalar(+5491112345678))))),List(),Some(SequenceReport(SequenceType(Unbounded,ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/))))),Some(Sequence(List(Scalar(+541143473177), Scalar(+5491112345678)))),List(),List(ScalarReport(ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/)))),Some(Scalar(+541143473177)),List()), ScalarReport(ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/)))),Some(Scalar(+5491112345678)),List())))))))))))

    val message = result1.message

    println(message) // Imprime: Record {  }

    val caso2 = record(
      "nombre" → 122,
      "cuit" → "1234678901a",
      "ojos" → sequence("verde", "azul", "naranja"),
      "domicilio" → record(
        "dirección" → "La Pampa 1234",
        "teléfonos" → sequence("+541143473177", "+5491112345678")
      )
    )

    val result2 = tipo check caso2

    println(result2.message) // Imprime: Record { Field "nombre" { Unexpected value Scalar of Int "122" when expecting Scalar of java.lang.String }, Field "cuit" { Condition "CUIT format" not met by 1234678901a }, Field "ojos" { Sequence (2 or less) { Cardinality 3 is greater than 2 } }, Field "domicilio" { Record { Field "cp" { Missing } } } }

  }

}
