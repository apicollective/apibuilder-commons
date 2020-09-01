package io.apibuilder.commons.types

// Represents a Scalar type in Api Builder
trait ScalarType {
  def name: String
}

class ScalarTypeImpl(
  override val name: String,
) extends ScalarType

object ScalarType {

  case object BooleanType extends ScalarTypeImpl("boolean")
  case object IntType extends ScalarTypeImpl("integer")
  case object StringType extends ScalarTypeImpl("string")
  case object DecimalType extends ScalarTypeImpl("decimal")
  case object FloatType extends ScalarTypeImpl("double")
  case object LongType extends ScalarTypeImpl("long")
  case object JsonType extends ScalarTypeImpl("json")
  case object ObjectType extends ScalarTypeImpl("object")
  case object DateIso8601Type extends ScalarTypeImpl("date-iso8601")
  case object DateTimeIso8601Type extends ScalarTypeImpl("date-time-iso8601")
  case object UnitType extends ScalarTypeImpl("unit")

  val all: scala.List[ScalarType] = scala.List(BooleanType, IntType, StringType, DecimalType, FloatType, LongType, JsonType, ObjectType, DateIso8601Type, DateTimeIso8601Type, UnitType)

  private[this] val byName: Map[String, ScalarType] = all.map(x => x.name.toLowerCase -> x).toMap

  def fromName(name: String): Option[ScalarType] = byName.get(name.toLowerCase)

}
