package suited

import org.scalautils.{ Bad, Every, Good, One, Or }
import org.scalautils.Accumulation._

import model._

object types {

  sealed trait Ok
  object Ok extends Ok

  type Result = Ok Or Every[Error]

  sealed trait Validator[T] {
    def validates(t: T): Result
  }

  sealed trait Error
  case class Invalid[T](declaration: Validator[T], value: T) extends Error
  case class Missing[T, U](declaration: Validator[T], from: U) extends Error
  case class Unexpected[T, U](declaration: Validator[T], value: U) extends Error

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

  case class ScalarType(predicate: Any ⇒ Boolean) extends ValueType {
    def validates(value: Value): Result =
      if (predicate(value)) Good(Ok) else Bad(One(Invalid(this, value)))
    override def toString = "ScalarType"
  }

  case class Segment(cardinality: CardinalityType, elementType: ValueType)

  case class SequenceType(segments: Segment*) extends ValueType {
    def validates(value: Value): Result =
      ???
  }

  case class FieldType(name: String, valueType: ValueType) extends Validator[Field] {
    def field(record: Record): Option[Field] =
      record.fields.find(_.name == name)
    def validates(field: Field): Result =
      if (field.name == name) valueType.validates(field.value)
      else Bad(One(Invalid(this, field)))
    def validates(record: Record): Result =
      field(record).map(validates).getOrElse(Bad(One(Missing(this, record))))
  }

  case class RecordType(fieldTypes: FieldType*) extends ValueType with UniqueFieldNames {
    def fieldNames = fieldTypes.toList.map(_.name)
    def validates(value: Value): Result =
      value match {
        case record: Record ⇒ fieldTypes.map(_.validates(record)).combined.map(_ ⇒ Ok)
        case other          ⇒ Bad(One(Unexpected(this, other)))
      }
  }

  case class FusionType(recordTypes: RecordType*) extends ValueType with UniqueFieldNames {
    def fieldNames = recordTypes.toList.flatMap(_.fieldNames)
    def validates(value: Value): Result =
      value match {
        case record: Record ⇒ recordTypes.map(_.validates(record)).combined.map(_ ⇒ Ok)
        case other          ⇒ Bad(One(Unexpected(this, other)))
      }
  }

  object validators {

    import dsl.ValidationFunctionOps

    val nonEmpty: String ⇒ Boolean = _.nonEmpty
    val isEmpty: String ⇒ Boolean = _.isEmpty
    val isInteger: String ⇒ Boolean = _.matches("""[+-]?\d+""")
    val isDecimal: String ⇒ Boolean = _.matches("""[+-]?\d+(\.\d+)?""")
    val isCUIT: String ⇒ Boolean = _.matches("""\d{11}""")
    val isCES: String ⇒ Boolean = _.matches("""[0A-Z]{6}[0-9]{4}""")
    val isIPv4: String ⇒ Boolean = _.matches("""((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)""")
    val isIPv6: String ⇒ Boolean = _.matches("""(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]).){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))""")
    val isIP: String ⇒ Boolean = isIPv4 or isIPv6

    def matches(pattern: String): String ⇒ Boolean = _.matches(pattern)
    def hasLength(n: Int ⇒ Boolean): String ⇒ Boolean = s ⇒ n(s.size)

    def asInt(f: Int ⇒ Boolean): String ⇒ Boolean = s ⇒ isInteger(s) && f(s.toInt)
    def asLong(f: Long ⇒ Boolean): String ⇒ Boolean = s ⇒ isInteger(s) && f(s.toLong)
    def asBigInt(f: BigInt ⇒ Boolean): String ⇒ Boolean = s ⇒ isInteger(s) && f(BigInt(s))
    def asBigDecimal(f: BigDecimal ⇒ Boolean): String ⇒ Boolean = s ⇒ isDecimal(s) && f(BigDecimal(s))

    def isLessThan[T](t: T)(implicit n: Numeric[T]): T ⇒ Boolean = n.lt(t, _)
    def isMoreThan[T](t: T)(implicit n: Numeric[T]): T ⇒ Boolean = n.gt(t, _)

  }

  object dsl {

    def of[T](f: T ⇒ Boolean)(implicit m: Manifest[T]): Any ⇒ Boolean =
      v ⇒ m.runtimeClass.isInstance(v) && f(v.asInstanceOf[T])

    def of[T](implicit m: Manifest[T]): Any ⇒ Boolean =
      v ⇒ m.runtimeClass.isInstance(v)

    def str(f: String ⇒ Boolean): Any ⇒ Boolean =
      of[String](f)

    implicit class ValidationFunctionOps[T](f: T ⇒ Boolean) {
      def and(g: T ⇒ Boolean): T ⇒ Boolean = v ⇒ f(v) && g(v)
      def or(g: T ⇒ Boolean): T ⇒ Boolean = v ⇒ f(v) || g(v)
    }

    implicit def validatorCanBeScalarType(validator: Any ⇒ Boolean): ScalarType = ScalarType(validator)
    implicit def fieldTypeCanBeRecordType(fieldType: FieldType): RecordType = RecordType(fieldType)
    implicit def segmentCanBeSequenceType(segment: Segment): SequenceType = SequenceType(segment)

    implicit class FieldTypeBuilder(name: String) {
      def is(scalarType: ScalarType) = FieldType(name, scalarType)
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
