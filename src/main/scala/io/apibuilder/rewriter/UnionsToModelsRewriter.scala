package apibuilder.rewriter

import cats.data.Validated.{Invalid, Valid}
import io.apibuilder.commons.MultiService
import io.apibuilder.commons.types.{ApiBuilderService, ApiBuilderType}
import io.apibuilder.commons.util.UnionToModel

/**
 * Replaces all union types with models (and their associated discriminator enums).
 * If the union type cannot be converted to a model - usually because we find types
 * with fields that themselves have types that are different - we leave that union
 * type untouched.
 */
case class UnionsToModelsRewriter(multiService: MultiService) extends Rewriter {

  private[this] val unionToModel: UnionToModel = UnionToModel(multiService)

  override def rewrite: MultiService = {
    MultiService(
      multiService.services().map(rewrite)
    )
  }

  def rewrite(service: ApiBuilderService): ApiBuilderService = {
    val newTypes = MultiService(List(service)).allUnions.flatMap(rewrite)
    service.copy(
      service = service.service.copy(
        unions = newTypes.collect { case e: ApiBuilderType.Union => e.union },
        enums = service.service.enums ++ newTypes.collect { case e: ApiBuilderType.Enum => e.enum },
        models = service.service.models ++ newTypes.collect { case m: ApiBuilderType.Model => m.model },
      )
    )
  }

  private[this] def rewrite(union: ApiBuilderType.Union): Seq[ApiBuilderType] = {
    unionToModel.toModel(union) match {
      case Valid(r) => r.apiBuilderTypes
      case Invalid(errors) => {
        // TODO: Maybe thread through validation end to end
        sys.error(s"rewrite errors: " + errors.toNonEmptyList.toList.mkString(", "))
      }
    }
  }

}
