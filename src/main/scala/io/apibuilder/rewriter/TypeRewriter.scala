package apibuilder.rewriter

import apibuilder.{ApiBuilderHelper, ApiBuilderHelperImpl}
import io.apibuilder.commons.MultiService
import io.apibuilder.spec.v0.models._
import io.apibuilder.commons.types.{ApiBuilderService, ApiBuilderType}

sealed trait ServiceType
object ServiceType {
  case class Scalar(name: String) extends ServiceType
  case class ApiBuilder(apiBuilderType: ApiBuilderType) extends ServiceType
}

/**
 * Provides the ability to rename the types of unions / models type (eg. replacing a union with object)
 *
 * @param rewriteType A function that is called for each type we find. You return the new type if you want to change it
 */
case class TypeRewriter(multiService: MultiService)(
  rewriteType: ServiceType => Option[ServiceType]
) {
  private[this] val helper: ApiBuilderHelper = ApiBuilderHelperImpl(multiService)

  def rewrite: MultiService = {
    MultiService(multiService.services.map(rewrite))
  }

  private[this] def rewrite(service: ApiBuilderService): ApiBuilderService = {
    ApiBuilderService(
      service = service.service.copy(
        enums = service.service.enums.map { m => rewriteEnum(ApiBuilderType.Enum(service.service, m)) }.map(_.`enum`),
        models = service.service.models.map { m => rewriteModel(ApiBuilderType.Model(service.service, m)) }.map(_.model),
        unions = service.service.unions.map { u => rewriteUnion(ApiBuilderType.Union(service.service, u)) }.map(_.union),
        resources = service.service.resources.map { r => rewrite(service.service, r) }
      )
    )
  }

  private[this] def rewrite(service: Service, resource: Resource): Resource = {
    resource.copy(
      operations = resource.operations.map { op =>
        rewrite(service, op)
      }
    )
  }

  private[this] def rewrite(service: Service, operation: Operation): Operation = {
    operation.copy(
      body = operation.body.map { b => rewrite(service, b) },
      parameters = operation.parameters.map { p => rewrite(service, p) },
      responses = operation.responses.map { r => rewrite(service, r) },
    )
  }

  private[this] def rewrite(service: Service, body: Body): Body = {
    body.copy(
      `type` = doRewriteType(service, body.`type`),
    )
  }

  private[this] def rewrite(service: Service, parameter: Parameter): Parameter = {
    parameter.copy(
      `type` = doRewriteType(service, parameter.`type`),
    )
  }

  private[this] def rewrite(service: Service, response: Response): Response = {
    response.copy(
      `type` = doRewriteType(service, response.`type`),
    )
  }

  private[this] def rewriteEnum(typ: ApiBuilderType.Enum): ApiBuilderType.Enum = {
    ApiBuilderType.Enum(
      typ.service,
      typ.`enum`.copy(
        name = doRewriteType(typ.service, typ.`enum`.name),
      )
    )
  }

  private[this] def rewriteUnion(typ: ApiBuilderType.Union): ApiBuilderType.Union = {
    ApiBuilderType.Union(
      typ.service,
      typ.union.copy(
        name = doRewriteType(typ.service, typ.union.name),
        types = typ.union.types.map { t =>
          t.copy(
            `type` = doRewriteType(typ.service, t.`type`),
          )
        }
      )
    )
  }

  private[this] def rewriteModel(typ: ApiBuilderType.Model): ApiBuilderType.Model = {
    ApiBuilderType.Model(
      typ.service,
      typ.model.copy(
        name = doRewriteType(typ.service, typ.model.name),
        fields = typ.model.fields.map { f =>
          f.copy(
            `type` = doRewriteType(typ.service, f.`type`)
          )
        }
      )
    )
  }

  private[this] def doRewriteType(service: Service, typeName: String): String = {
    val baseType = helper.resolveType(service, typeName) match {
      case None => ServiceType.Scalar(typeName)
      case Some(t) => ServiceType.ApiBuilder(t)
    }

    rewriteType(baseType) match {
      case None => {
        typeName
      }
      case Some(nt) => {
        addCollections(
          typeName,
          nt match {
            case t: ServiceType.Scalar => t.name
            case t: ServiceType.ApiBuilder if t.apiBuilderType.namespace == service.namespace => t.apiBuilderType.name
            case t: ServiceType.ApiBuilder => t.apiBuilderType.qualified
          }
        )
      }
    }
  }

  private[this] def addCollections(originalType: String, newType: String): String = {
    originalType match {
      case ApiBuilderHelper.Array(inner) => "[" + addCollections(inner, newType) + "]"
      case ApiBuilderHelper.Map(inner) => "map[" + addCollections(inner, newType) + "]"
      case _ => newType
    }
  }
}
