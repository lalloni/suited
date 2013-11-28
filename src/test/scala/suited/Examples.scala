package suited

import org.scalatest._

import suited.model._
import suited.model.dsl._
import suited.types.dsl._
import suited.types.validators._

class Examples extends FunSuite with Matchers {

  test("Examples") {

    val tipo1 = (
      ("nombre" is nonEmpty) ~
      ("cuit" is CUIT) ~
      ("ojos" are (2 orLess any[String])) ~
      ("domicilio" is
        ("dirección" is (any[String] and hasLength(greaterThan(5)))) ~
        ("cp" is (any[Int] and greaterThan(999) and lesserThan(10000))) ~
        ("teléfonos" are (many of (any[String] and nonEmpty and matches("\\+.*"))))
      )
    )

    println(tipo1) // Imprime: RecordType(WrappedArray(FieldType(nombre,ScalarType(Some(Condition(Non empty)))), FieldType(cuit,ScalarType(Some(Condition(CUIT format)))), FieldType(ojos,SequenceType(EqualOrLess(2),ScalarType(Some(Type(TypeTag[String]))))), FieldType(domicilio,RecordType(WrappedArray(FieldType(dirección,ScalarType(Some(And(Type(TypeTag[String]),View(Length,Condition(More than 5)))))), FieldType(cp,ScalarType(Some(And(And(Type(TypeTag[Int]),Condition(More than 999)),Condition(Less than 10000))))), FieldType(teléfonos,SequenceType(Unbounded,ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/)))))))))))

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

    val result1 = tipo1 check caso1

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

    val result2 = tipo1 check caso2

    println(result2.message) // Imprime: Record { Field "nombre" { Unexpected value Scalar of Int "122" when expecting Scalar of java.lang.String }, Field "cuit" { Condition "CUIT format" not met by 1234678901a }, Field "ojos" { Sequence (2 or less) { Cardinality 3 is greater than 2 } }, Field "domicilio" { Record { Field "cp" { Missing } } } }

    val tipo2 = ("agencia" is any[Int]) ~ ("región" is any[Int])

    println(tipo2)

    val tipo1y2 = tipo1 + tipo2 // Fusión de tipos de registros ("herencia múltiple")

    println(tipo1y2) // Imprime: FusionType(WrappedArray(RecordType(WrappedArray(FieldType(nombre,ScalarType(Some(Condition(Non empty)))), FieldType(cuit,ScalarType(Some(Condition(CUIT format)))), FieldType(ojos,SequenceType(EqualOrLess(2),ScalarType(Some(Type(TypeTag[String]))))), FieldType(domicilio,RecordType(WrappedArray(FieldType(dirección,ScalarType(Some(And(Type(TypeTag[String]),View(Length,Condition(More than 5)))))), FieldType(cp,ScalarType(Some(And(And(Type(TypeTag[Int]),Condition(More than 999)),Condition(Less than 10000))))), FieldType(teléfonos,SequenceType(Unbounded,ScalarType(Some(And(And(Type(TypeTag[String]),Condition(Non empty)),Condition(Matches /\+.*/))))))))))), RecordType(WrappedArray(FieldType(agencia,ScalarType(Some(Type(TypeTag[Int])))), FieldType(región,ScalarType(Some(Type(TypeTag[Int]))))))))

    val result3 = tipo1y2 check caso1

    println(result3.message) // Imprime: Fusion { Record 1 { Record { Field "agencia" { Missing }, Field "región" { Missing } } } }

    val result4 = tipo1y2 check (caso1 ++ ("agencia" → 11, "región" → 2))

    println(result4.message) // Imprime: Fusion {  }

    val tipo3 = (
      ("algo" is anyRecord) ~
      ("cosas" are (many of anyValue)) ~
      ("valor" is anyValue)
    )

    println(tipo3.check(record("algo" → record(), "cosas" → sequence("a", 2), "valor" → 0)).message) // Imprime: Record {  }

    println(tipo3.check(record("algo" → 1, "cosas" → 2)).message) // Imprime: Record { Field "algo" { Record { Fault(Unexpected value Scalar of Int "1" when expecting a Record) } }, Field "cosas" { Sequence (Unbounded) { Unexpected value Scalar of Int "2" when expecting a Sequence } }, Field "valor" { Missing } }

  }

}
