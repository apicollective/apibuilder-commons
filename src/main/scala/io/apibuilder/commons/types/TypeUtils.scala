package io.apibuilder.commons.types

import io.apibuilder.commons.MultiService
import io.apibuilder.spec.v0.models._
import io.apibuilder.commons.types.{ApiBuilderService, ApiBuilderType}

import scala.annotation.tailrec
import scala.util.matching.Regex

case class TypeUtilsImpl(override val multiService: MultiService) extends TypeUtils

object TypeUtils {
  val Array: Regex = """^\[(.+)\]$""".r
  val Map: Regex = """^map\[(.+)\]$""".r

  def is2xx(r: Response): Boolean = {
    r.code match {
      case ResponseCodeInt(v) => v >= 200 && v < 300
      case ResponseCodeOption.Default => true
      case ResponseCodeOption.UNDEFINED(_) | ResponseCodeUndefinedType(_) => false
    }
  }
}

trait TypeUtils {

  def multiService: MultiService

  def resolveType(service: ApiBuilderService, typeName: String): Option[ApiBuilderType] = {
    resolveType(service.service, typeName)
  }

  def resolveType(service: Service, resource: Resource): Option[ApiBuilderType] = resolveType(service, resource.`type`)
  def resolveType(service: Service, body: Body): Option[ApiBuilderType] = resolveType(service, body.`type`)
  def resolveType(service: Service, parameter: Parameter): Option[ApiBuilderType] = resolveType(service, parameter.`type`)

  def resolveType(service: Service, typeName: String): Option[ApiBuilderType] = {
    multiService.findType(
      defaultNamespace = service.namespace,
      typeName = baseType(service, typeName),
    )
  }

  def isDeprecated(resource: Resource): Boolean = resource.deprecation.isDefined

  def isDeprecated(operation: Operation): Boolean = operation.deprecation.isDefined

  def isDeprecated(response: Response): Boolean = response.deprecation.isDefined

  def isMap(typeName: String): Boolean = {
    typeName match {
      case TypeUtils.Map(_) => true
      case _ => false
    }
  }

  def isArray(typeName: String): Boolean = {
    typeName match {
      case TypeUtils.Array(_) => true
      case _ => false
    }
  }

  def responseIsMulti(operation: Operation): Boolean = {
    operation.responses.filter(TypeUtils.is2xx).exists(responseIsMulti)
  }

  def responseIsMulti(response: Response): Boolean = {
    isMap(response.`type`) || isArray(response.`type`)
  }

  def responseIsMulti(responseType: String): Boolean = {
    isMap(responseType) || isArray(responseType)
  }

  final def isScalar(service: ApiBuilderService, typeName: String): Boolean = {
    ScalarType.fromApiBuilderType(baseType(service.service, typeName)).isDefined
  }

  def isModel(apiBuilderService: ApiBuilderService, typeName: String): Boolean = {
    isModel(apiBuilderService.service, typeName)
  }

  def isModel(service: Service, typeName: String): Boolean = {
    resolveType(service, typeName) match {
      case Some(_: ApiBuilderType.Model) => true
      case _ => false
    }
  }

  @tailrec
  final def baseType(service: Service, typeName: String): String = {
    typeName match {
      case TypeUtils.Array(inner) => baseType(service, inner)
      case TypeUtils.Map(inner) => baseType(service, inner)
      case _ => typeName
    }
  }

}
