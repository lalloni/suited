package suited

import scala.reflect.runtime.{ universe ⇒ u }
import u.TypeTag

import suited.model._

import suited.types.validators.{ Validator, Good, Bad }

package types {

  sealed trait CardinalityType {
    def validates(count: Int): Boolean
    def check(count: Int): Option[Fault]
  }

  case object Unbounded extends CardinalityType {
    def validates(count: Int) = true
    def check(count: Int) = None
  }

  case class Exactly(quantity: Int) extends CardinalityType {
    def validates(count: Int) = count == quantity
    def check(count: Int) = if (validates(count)) None else Some(Fault(s"Cardinality is not $quantity", count))
  }

  case class AtMost(quantity: Int) extends CardinalityType {
    def validates(count: Int) = count <= quantity
    def check(count: Int) = if (validates(count)) None else Some(Fault(s"Cardinality is greater than $quantity", count))
  }

  case class AtLeast(quantity: Int) extends CardinalityType {
    def validates(count: Int) = count >= quantity
    def check(count: Int) = if (validates(count)) None else Some(Fault(s"Cardinality is lesser than $quantity", count))
  }

  case class Fault(message: String, evidence: Any)

  sealed trait Report {
    type ReportValue <: Value
    type ReportValueType <: ValueType
    def source: ValueType
    def target: Option[Value]
    def failed: Boolean
    def faults: List[Fault]
    def passed: Boolean = !failed
  }

  sealed trait ValueType {
    type ReportType <: Report
    def check(value: Value): ReportType
  }

  case class ScalarReport[S: Support](source: ScalarType[S], target: Option[Value], faults: List[Fault]) extends Report {
    type ReportValue = Scalar[S]
    type RepoteValueType = ScalarType[ReportValue]
    def failed = faults.nonEmpty
  }

  case class ScalarType[S: Support](validator: Option[Validator[S]])(implicit m: Manifest[S]) extends ValueType {
    type ReportType = ScalarReport[S]
    val runtimeClass = Helpers.weirdness(m.runtimeClass)
    def check(value: Value) = {
      value match {
        case Scalar(v) if runtimeClass.isInstance(v) ⇒
          ScalarReport(this, Some(value), validator.toList.flatMap(_(v.asInstanceOf[S]) match {
            case Good        ⇒ Nil
            case Bad(faults) ⇒ faults
          }))
        case other ⇒ ScalarReport(this, Some(value), List(Fault("Unexpected value", value)))
      }
    }
  }

  case class SequenceReport(source: SequenceType, target: Option[Value], faults: List[Fault], reports: List[Report]) extends Report {
    type ReportValue = Sequence
    type ReportValueType = SequenceType
    def failed = faults.nonEmpty || reports.exists(_.failed)
  }

  case class SequenceType(cardinality: CardinalityType, elementType: ValueType) extends ValueType {
    type ReportType = SequenceReport
    def check(value: Value) =
      value match {
        case Sequence(values) ⇒ SequenceReport(this, Some(value), cardinality.check(values.size).toList, values.map(elementType.check).toList)
        case other            ⇒ SequenceReport(this, Some(value), List(Fault(s"Unexpected value", value)), List.empty)
      }
  }

  case class FieldReport[T <: ValueType](source: FieldType[T], target: Option[Field], faults: List[Fault], valueReport: Option[T#ReportType]) {
    def failed = faults.nonEmpty || valueReport.exists(_.failed)
  }

  case class FieldType[T <: ValueType](name: String, valueType: T) {
    def check(field: Field): FieldReport[T] =
      FieldReport(this, Some(field), List.empty, Some(valueType.check(field.value)))
    def check(fields: List[Field]): FieldReport[T] =
      fields.find(_.name == name).fold(FieldReport(this, None, List(Fault(s"Missing '$name' field", fields)), None))(check)
  }

  case class RecordReport(source: RecordType, target: Option[Value], faults: List[Fault], reports: List[FieldReport[_]]) extends Report {
    type ReportValue = Record
    type ReportValueType = RecordType
    def failed = faults.nonEmpty || reports.exists(_.failed)
  }

  case class RecordType(fieldTypes: FieldType[_]*) extends ValueType with UniqueFieldNames {
    type ReportType = RecordReport
    def fieldNames = fieldTypes.toList.map(_.name)
    def check(value: Value) =
      value match {
        case Record(fields) ⇒ RecordReport(this, Some(value), List.empty, fieldTypes.map(_.check(fields)).toList)
        case other          ⇒ RecordReport(this, Some(value), List(Fault(s"Unexpected value", value)), List.empty)
      }
  }

  case class FusionReport(source: FusionType, target: Option[Value], faults: List[Fault], reports: List[RecordReport]) extends Report {
    type ReportValue = Record
    type ReportValueType = FusionType
    def failed = faults.nonEmpty || reports.exists(_.failed)
  }

  case class FusionType(recordTypes: RecordType*) extends ValueType with UniqueFieldNames {
    type ReportType = FusionReport
    def fieldNames = recordTypes.toList.flatMap(_.fieldNames)
    def check(value: Value) =
      value match {
        case Record(fields) ⇒ FusionReport(this, Some(value), Nil, recordTypes.map(_.check(value)).toList)
        case other          ⇒ FusionReport(this, Some(value), List(Fault(s"Unexpected value", value)), List.empty)
      }
  }

  object validators {

    sealed trait Result {
      def &&(other: Result): Result
      def ||(other: Result): Result
      def map(f: Fault ⇒ Fault): Result
    }

    case object Good extends Result {
      def &&(other: Result): Result = other
      def ||(other: Result): Result = this
      def map(f: Fault ⇒ Fault): Result = this
    }

    case class Bad(faults: List[Fault]) extends Result {
      def &&(other: Result): Result = other match {
        case Good             ⇒ this
        case Bad(otherFaults) ⇒ Bad(faults ++ otherFaults)
      }
      def ||(other: Result): Result = other match {
        case Good             ⇒ Good
        case Bad(otherFaults) ⇒ Bad(faults ++ otherFaults)
      }
      def map(f: Fault ⇒ Fault): Result =
        Bad(faults.map(f))
    }

    sealed trait Validator[T] {
      def apply(value: T): Result
    }

    implicit class ValidatorOps[T](self: Validator[T]) {
      def and(other: Validator[T]): Validator[T] = And(self, other)
      def or(other: Validator[T]): Validator[T] = Or(self, other)
    }

    case class Condition[T](name: String)(condition: T ⇒ Boolean) extends Validator[T] {
      def apply(value: T): Result =
        if (condition(value)) Good else Bad(List(Fault(s"Condition not met: $name", value)))
    }

    case class And[T](a: Validator[T], b: Validator[T]) extends Validator[T] {
      def apply(value: T): Result = a(value) && b(value)
    }

    case class Or[T](a: Validator[T], b: Validator[T]) extends Validator[T] {
      def apply(value: T): Result = a(value) || b(value)
    }

    case class View[T, V](name: String, validator: Validator[V])(view: T ⇒ V) extends Validator[T] {
      def apply(value: T): Result =
        validator.apply(view(value)).map(f ⇒ Fault(s"As $name: ${f.message}", f.evidence))
    }

    case class Type[S: Support](implicit m: Manifest[S]) extends Validator[S] {
      val runtimeClass = Helpers.weirdness(m.runtimeClass)
      def apply(value: S): Result =
        if (runtimeClass.isInstance(value)) Good else Bad(List(Fault("Unexpected value type", value)))
    }

    def any[S: Support: Manifest]: Validator[S] = Type[S]
    def validate[T](name: String)(predicate: T ⇒ Boolean): Validator[T] = Condition(name)(predicate)

    val nonEmpty: Validator[String] = validate[String]("Non empty")(_.nonEmpty)
    val isEmpty: Validator[String] = validate[String]("Empty")(_.isEmpty)
    val isInteger: Validator[String] = validate[String]("Integer format")(_.matches("""[+-]?\d+"""))
    val isDecimal: Validator[String] = validate[String]("Decimal format")(_.matches("""[+-]?\d+(\.\d+)?"""))
    val isCUIT: Validator[String] = validate[String]("CUIT format")(_.matches("""\d{11}"""))
    val isCES: Validator[String] = validate[String]("CES format")(_.matches("""[0A-Z]{6}[0-9]{4}"""))
    val isIPv4: Validator[String] = validate[String]("IPv4 format")(_.matches("""((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"""))
    val isIPv6: Validator[String] = validate[String]("IPv6 format")(_.matches("""(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))"""))
    val isIP: Validator[String] = isIPv4 or isIPv6

    def matches(pattern: String): Validator[String] = validate[String](s"Matches /$pattern/")(_.matches(pattern))

    def lessThan[N](number: N)(implicit n: Numeric[N]): Validator[N] = validate(s"Less than $number")(n.lt(number, _))
    def moreThan[N](number: N)(implicit n: Numeric[N]): Validator[N] = validate(s"More than $number")(n.gt(number, _))

    def hasLength(validator: Validator[Int]): Validator[String] = View("Length", validator)(_.size)

    def int(f: Validator[Int]): Validator[String] = isInteger and View("Int", f)(_.toInt)
    def long(f: Validator[Long]): Validator[String] = isInteger and View("Long", f)(_.toLong)
    def bigInt(f: Validator[BigInt]): Validator[String] = isInteger and View("BigInt", f)(BigInt(_))
    def bigDecimal(f: Validator[BigDecimal]): Validator[String] = isDecimal and View("BigDecimal", f)(BigDecimal(_))

    implicit def validatorCanBeScalarType[T: Manifest: Support](validator: Validator[T]): ScalarType[T] =
      ScalarType(Some(validator))

  }

  object dsl {

    implicit def fieldTypeCanBeRecordType[T <: ValueType](fieldType: FieldType[T]): RecordType = RecordType(fieldType)

    implicit class FieldTypeBuilder(name: String) {
      def is[S: Support](scalarType: ScalarType[S]): FieldType[ScalarType[S]] = FieldType(name, scalarType)
      def is(recordType: RecordType): FieldType[RecordType] = FieldType(name, recordType)
      def is(fusionType: FusionType): FieldType[FusionType] = FieldType(name, fusionType)
      def are(sequenceType: SequenceType): FieldType[SequenceType] = FieldType(name, sequenceType)
    }

    implicit class RecordTypeBuilder1[T <: ValueType](fieldType: FieldType[T]) {
      def ~[T1 <: ValueType](otherFieldType: FieldType[T1]): RecordType = RecordType(fieldType, otherFieldType)
    }

    implicit class RecordTypeBuilder2(recordType: RecordType) {
      def ~[T <: ValueType](fieldType: FieldType[T]): RecordType = RecordType((recordType.fieldTypes :+ fieldType).toArray: _*)
      def +(otherRecordType: RecordType): FusionType = FusionType(recordType, otherRecordType)
    }

    implicit class FusionTypeBuilder1(fusionType: FusionType) {
      def +(otherRecordType: RecordType): FusionType = FusionType((fusionType.recordTypes :+ otherRecordType).toArray: _*)
    }

    implicit class SequenceBuilder1(quantity: Int) {
      def are(valueType: ValueType): SequenceType = SequenceType(Exactly(quantity), valueType)
      def orLess(valueType: ValueType): SequenceType = SequenceType(AtMost(quantity), valueType)
      def orMore(valueType: ValueType): SequenceType = SequenceType(AtLeast(quantity), valueType)
    }

    val many = new {
      def are(valueType: ValueType): SequenceType = SequenceType(Unbounded, valueType)
    }

  }

  private[types] object Helpers {
    val weirdness: Map[Class[_], Class[_]] = Map[Class[_], Class[_]](
      classOf[Integer] → classOf[Int],
      Integer.TYPE → classOf[Int]
    ).withDefault(identity)
  }
}
