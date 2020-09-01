package apibuilder.rewriter

import io.apibuilder.commons.MultiService
import io.apibuilder.spec.v0.models.{Service, Union, UnionType}
import io.apibuilder.commons.types.ApiBuilderService

/**
 * Modify the types of each union. If the resulting types are empty, the
 * union type itself is removed and any model fields that referenced this
 * union will have their type changed to 'object'
 *
 * @param rewriteTypes Called for each union. Return the types that you'd like to keep
 */
case class UnionTypeRewriter(multiService: MultiService)(
  rewriteTypes: ApiBuilderType.Union => Seq[UnionType]
) extends Rewriter {

  override def rewrite: MultiService = {
    val ms = rewriteUnionTypes(multiService)
    val emptyUnions = ms.allUnions.filter(_.union.types.isEmpty)
    removeEmptyUnionTypes(
      renameToObject(ms, emptyUnions)
    )
  }

  private[this] def rewriteUnionTypes(multiService: MultiService): MultiService = {
    MultiService(
      multiService.services().map { s =>
        rewriteUnionTypes(s)
      }
    )
  }

  private[this] def rewriteUnionTypes(apiBuilderService: ApiBuilderService): ApiBuilderService = {
    apiBuilderService.copy(
      service = apiBuilderService.service.copy(
        unions = apiBuilderService.service.unions.map { u =>
          rewriteUnionTypes(apiBuilderService.service, u)
        }
      )
    )
  }

  private[this] def rewriteUnionTypes(service: Service, union: Union): Union = {
    union.copy(
      types = rewriteTypes(ApiBuilderType.Union(service, union))
    )
  }

  private[this] def removeEmptyUnionTypes(multiService: MultiService): MultiService = {
    MultiService(
      multiService.services().map { s =>
        removeEmptyUnionTypes(s)
      }
    )
  }

  private[this] def removeEmptyUnionTypes(apiBuilderService: ApiBuilderService): ApiBuilderService = {
    apiBuilderService.copy(
      service = apiBuilderService.service.copy(
        unions = apiBuilderService.service.unions.filterNot(_.types.isEmpty),
      )
    )
  }

  private[this] def renameToObject(multiService: MultiService, types: Seq[ApiBuilderType.Union]): MultiService = {
    val names = types.map(_.qualified).toSet
    TypeRewriter(multiService) {
      case ServiceType.ApiBuilder(t) if names.contains(t.qualified) => Some(
        ServiceType.Scalar(ScalarType.ObjectType.apiBuilderType)
      )
      case _: ServiceType.ApiBuilder | _: ServiceType.Scalar => None
    }.rewrite
  }

}
