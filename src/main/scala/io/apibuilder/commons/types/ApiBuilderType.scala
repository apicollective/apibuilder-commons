package io.apibuilder.commons.types

import io.apibuilder.commons.ApiBuilderService
import io.apibuilder.spec.v0.models

sealed trait TypeDiscriminator
object TypeDiscriminator {
  case object Enums extends TypeDiscriminator { override def toString = "enums"}
  case object Models extends TypeDiscriminator { override def toString = "models"}
  case object Unions extends TypeDiscriminator { override def toString = "unions"}
}

sealed trait ApiBuilderType {

  def service: ApiBuilderService
  def name: String
  def typeDiscriminator: TypeDiscriminator

  def namespace: String = service.namespace

  /**
   * returns the fully qualified name of this type (ie including the namespace)
   */
  def qualified: String = s"$namespace.$typeDiscriminator.$name"
}

object ApiBuilderType {
  case class Enum(override val service: ApiBuilderService, enum: models.Enum) extends ApiBuilderType {
    override val name: String = enum.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Enums
  }
  case class Model(override val service: ApiBuilderService, model: models.Model) extends ApiBuilderType {
    override val name: String = model.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Models
  }
  case class Union(override val service: ApiBuilderService, union: models.Union) extends ApiBuilderType {
    override val name: String = union.name
    override val typeDiscriminator: TypeDiscriminator = TypeDiscriminator.Unions
  }
}
