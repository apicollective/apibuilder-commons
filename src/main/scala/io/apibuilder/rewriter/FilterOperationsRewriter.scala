package apibuilder.rewriter

import io.apibuilder.commons.MultiService
import io.apibuilder.spec.v0.models.{Operation, Resource}
import io.apibuilder.commons.types.ApiBuilderService

/**
 * Rewrite the resources and operations based on an accepts filter.
 * If the corresponding resource has no operations, it is removed.
 */
case class FilterOperationsRewriter(multiService: MultiService)(
  rewrite: Operation => Option[Operation],
) extends Rewriter {

  override def rewrite: MultiService = {
    MultiService(
      multiService.services().map(rewrite)
    )
  }

  def rewrite(apiBuilderService: ApiBuilderService): ApiBuilderService = {
    apiBuilderService.copy(
      service = apiBuilderService.service.copy(
        resources = apiBuilderService.service.resources.flatMap(filter),
      )
    )
  }

  private[this] def filter(resource: Resource): Option[Resource] = {
    resource.operations.flatMap(rewrite).toList match {
      case Nil => None
      case ops => Some(resource.copy(operations = ops))
    }
  }

}
