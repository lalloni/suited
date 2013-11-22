package suited

import org.scalautils.{ Bad, Every, Good, One, Or }
import org.scalautils.Accumulation._

import model._
import types._

object types {

  sealed trait Ok
  case object Ok extends Ok

  type Result = Ok Or Every[Error]

  sealed trait Validator[T] {
    def apply(value: T): Result
  }

  sealed trait Error

  case class Invalid[T](value: T, declaration: Validator[T]) extends Error
  case class Unexpected[T, U](value: U, declaration: Validator[T]) extends Error
  case class Missing[T, U](from: U, declaration: Validator[T]) extends Error

  sealed trait CardinalityType {
    def validates(count: Int): Boolean
  }

  case object Unbounded extends CardinalityType {
    def validates(count: Int) = true
  }

  case class Exactly(quantity: Int) extends CardinalityType {
    def validates(count: Int) = count == quantity
  }

  case class AtMost(quantity: Int) extends CardinalityType {
    def validates(count: Int) = count <= quantity
  }

  case class AtLeast(quantity: Int) extends CardinalityType {
    def validates(count: Int) = count >= quantity
  }

  sealed trait ValueType extends Validator[Value]

  case class ScalarType[T: Supported](validator: Validator[T])(implicit m: Manifest[T]) extends ValueType {
    def apply(value: Value): Result =
      value match {
        case Scalar(v) if m.runtimeClass.isInstance(v) ⇒
          validator.apply(v.asInstanceOf[T]) match {
            case Bad(errors) ⇒ Bad(One(Invalid(value, this)))
            case good        ⇒ good
          }
        case other ⇒ Bad(One(Unexpected(value, this)))
      }
  }

  case class Segment(cardinality: CardinalityType, elementType: ValueType)

  case class SequenceType(segments: Segment*) extends ValueType {
    def apply(value: Value): Result =
      ???
  }

  case class FieldType(name: String, valueType: ValueType) extends Validator[Field] {
    def field(record: Record): Option[Field] =
      record.fields.find(_.name == name)
    def apply(record: Record): Result =
      field(record).map(apply).getOrElse(Bad(One(Missing(record, this))))
    def apply(field: Field): Result =
      if (field.name == name) valueType.apply(field.value)
      else Bad(One(Invalid(field, this)))
  }

  case class RecordType(fieldTypes: FieldType*) extends ValueType with UniqueFieldNames {
    def fieldNames = fieldTypes.toList.map(_.name)
    def apply(value: Value): Result =
      value match {
        case record: Record ⇒ fieldTypes.map(_.apply(record)).combined.map(_ ⇒ Ok)
        case other          ⇒ Bad(One(Unexpected(other, this)))
      }
  }

  case class FusionType(recordTypes: RecordType*) extends ValueType with UniqueFieldNames {
    def fieldNames = recordTypes.toList.flatMap(_.fieldNames)
    def apply(value: Value): Result =
      value match {
        case record: Record ⇒ recordTypes.map(_.apply(record)).combined.map(_ ⇒ Ok)
        case other          ⇒ Bad(One(Unexpected(other, this)))
      }
  }

  object validators {

    implicit class ValidatorOps[T](self: Validator[T]) {
      def and(other: Validator[T]): Validator[T] = AndValidator(self, other)
      def or(other: Validator[T]): Validator[T] = OrValidator(self, other)
    }

    case class PredicateValidator[T](description: String)(predicate: T ⇒ Boolean) extends Validator[T] {
      def apply(value: T): Result =
        if (predicate(value)) Good(Ok) else Bad(One(Invalid(value, this)))
    }

    case class AndValidator[T](a: Validator[T], b: Validator[T]) extends Validator[T] {
      def apply(value: T): Result = Seq(a(value), b(value)).combined.map(_ ⇒ Ok)
    }

    case class OrValidator[T](a: Validator[T], b: Validator[T]) extends Validator[T] {
      def apply(value: T): Result = Seq(a(value), b(value)).combined.map(_ ⇒ Ok)
    }

    case class TypeValidator[T: Supported](implicit m: Manifest[T]) extends Validator[T] {
      def apply(value: T): Result =
        if (m.runtimeClass.isInstance(value)) Good(Ok) else Bad(One(Unexpected(value, this)))
    }

    case class ViewValidator[T: Supported, V](description: String, validator: Validator[V])(view: T ⇒ V) extends Validator[T] {
      def apply(value: T): Result =
        validator.apply(view(value))
    }

    def validate[T](name: String)(predicate: T ⇒ Boolean): Validator[T] = PredicateValidator(name)(predicate)

    def any[T: Supported: Manifest]: Validator[T] = TypeValidator[T]

    val nonEmpty: Validator[String] = validate[String]("Non Empty")(_.nonEmpty)
    val isEmpty: Validator[String] = validate[String]("Empty")(_.isEmpty)
    val isInteger: Validator[String] = validate[String]("Integer")(_.matches("""[+-]?\d+"""))
    val isDecimal: Validator[String] = validate[String]("Decimal")(_.matches("""[+-]?\d+(\.\d+)?"""))
    val isCUIT: Validator[String] = validate[String]("CUIT")(_.matches("""\d{11}"""))
    val isCES: Validator[String] = validate[String]("CES")(_.matches("""[0A-Z]{6}[0-9]{4}"""))
    val isIPv4: Validator[String] = validate[String]("IPv4")(_.matches("""((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"""))
    val isIPv6: Validator[String] = validate[String]("IPv6")(_.matches("""(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))"""))
    val isIP: Validator[String] = isIPv4 or isIPv6

    def matches(pattern: String): Validator[String] = validate[String](s"Matches $pattern")(_.matches(pattern))

    def lessThan[N](number: N)(implicit n: Numeric[N]): Validator[N] = validate(s"LessThan($number)")(n.lt(number, _))
    def moreThan[N](number: N)(implicit n: Numeric[N]): Validator[N] = validate(s"MoreThan($number)")(n.gt(number, _))

    def hasLength(validator: Validator[Int]): Validator[String] = ViewValidator("Length", validator)(_.size)

    def int(f: Validator[Int]): Validator[String] = isInteger and ViewValidator("Int", f)(_.toInt)
    def long(f: Validator[Long]): Validator[String] = isInteger and ViewValidator("Long", f)(_.toLong)
    def bigInt(f: Validator[BigInt]): Validator[String] = isInteger and ViewValidator("BigInt", f)(BigInt(_))
    def bigDecimal(f: Validator[BigDecimal]): Validator[String] = isDecimal and ViewValidator("BigDecimal", f)(BigDecimal(_))

  }

  object dsl {

    implicit def validatorCanBeScalarType[T: Manifest: Supported](validator: Validator[T]): ScalarType[T] = ScalarType(validator)
    implicit def fieldTypeCanBeRecordType(fieldType: FieldType): RecordType = RecordType(fieldType)
    implicit def segmentCanBeSequenceType(segment: Segment): SequenceType = SequenceType(segment)

    implicit class FieldTypeBuilder(name: String) {
      def is[T](scalarType: ScalarType[T]) = FieldType(name, scalarType)
      def is(recordType: RecordType) = FieldType(name, recordType)
      def is(fusionType: FusionType) = FieldType(name, fusionType)
      def are(sequenceType: SequenceType) = FieldType(name, sequenceType)
    }

    implicit class RecordTypeBuilder1(fieldType: FieldType) {
      def ~(otherFieldType: FieldType): RecordType = RecordType(fieldType, otherFieldType)
    }

    implicit class RecordTypeBuilder2(recordType: RecordType) {
      def ~(fieldType: FieldType): RecordType = RecordType((recordType.fieldTypes :+ fieldType).toArray: _*)
      def +(otherRecordType: RecordType): FusionType = FusionType(recordType, otherRecordType)
    }

    implicit class FusionTypeBuilder1(fusionType: FusionType) {
      def +(otherRecordType: RecordType): FusionType = FusionType((fusionType.recordTypes :+ otherRecordType).toArray: _*)
    }

    implicit class SegmentBuilder1(quantity: Int) {
      def are(valueType: ValueType): Segment = Segment(Exactly(quantity), valueType)
      def orLess(valueType: ValueType): Segment = Segment(AtMost(quantity), valueType)
      def orMore(valueType: ValueType): Segment = Segment(AtLeast(quantity), valueType)
    }

    val many = new {
      def are(valueType: ValueType): Segment = Segment(Unbounded, valueType)
    }

    val one = new {
      def is(valueType: ValueType): Segment = Segment(Exactly(1), valueType)
    }

    implicit class SequenceTypeBuilder2(segment: Segment) {
      def &(otherSegment: Segment): SequenceType = SequenceType(segment, otherSegment)
    }

    implicit class SequenceTypeBuilder3(sequenceType: SequenceType) {
      def &(segment: Segment): SequenceType = SequenceType((sequenceType.segments :+ segment).toArray: _*)
    }

  }

}
