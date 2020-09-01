package apibuilder.rewriter

import io.apibuilder.commons.MultiService
import io.apibuilder.spec.v0.models.{Operation, Resource, Response}
import io.apibuilder.commons.types.ApiBuilderService

/**
 * Rewrite the responses based on an accepts filter.
 *   - if the corresponding operation ends up with no resources, it is removed
 *   - if the corresponding resource ends up with no operations, it is removed
 */
case class FilterResponsesRewriter(multiService: MultiService)(
  filterResponses: Operation => Seq[Response],
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
    resource.operations.flatMap(filter).toList match {
      case Nil => None
      case ops => Some(resource.copy(operations = ops))
    }
  }

  private[this] def filter(operation: Operation): Option[Operation] = {
    filterResponses(operation).toList match {
      case Nil => None
      case responses => Some(
        operation.copy(responses = responses)
      )
    }
  }

}
