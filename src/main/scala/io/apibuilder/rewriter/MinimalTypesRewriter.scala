package apibuilder.rewriter

import apibuilder.{ApiBuilderHelper, ApiBuilderHelperImpl}
import io.apibuilder.commons.MultiService
import io.apibuilder.commons.types.ApiBuilderType

import scala.annotation.tailrec

/**
 * Rewrites the multi service such that the specified types resolve correctly. eg.
 * recurses through the provides types to collect ALL referenced types (through models, unions, etc)
 * and return a multi service containing only the types necessary such that the provided
 * types fully resolve.
 */
case class MinimalTypesRewriter(multiService: MultiService, types: Iterable[ApiBuilderType]) extends Rewriter {

  private[this] val helper: ApiBuilderHelper = ApiBuilderHelperImpl(multiService)

  override def rewrite: MultiService = {
    val all = expand(types.toSeq, Set.empty).groupBy(_.namespace)

    MultiService(
      multiService.services().map { s =>
        val svcTypes = all.getOrElse(s.namespace, Nil).toSeq
        s.copy(
          service = s.service.copy(
            enums = svcTypes.collect { case t: ApiBuilderType.Enum => t }.map(_.`enum`),
            models = svcTypes.collect { case t: ApiBuilderType.Model => t }.map(_.model),
            unions = svcTypes.collect { case t: ApiBuilderType.Union => t }.map(_.union),
          )
        )
      }
    )
  }

  /**
   * Expand the types to include any types defined on the fields of models
   * or the types of a union
   */
  @tailrec
  private[this] def expand(incoming: Seq[ApiBuilderType], resolved: Set[ApiBuilderType]): Set[ApiBuilderType] = {
    incoming.toList match {
      case Nil => resolved
      case one :: rest => {
        val newTypes = one match {
          case _: ApiBuilderType.Enum => Nil
          case t: ApiBuilderType.Model => t.model.fields.flatMap { f => helper.resolveType(t.service, f.`type`) }
          case t: ApiBuilderType.Union => t.union.types.flatMap { ut => helper.resolveType(t.service, ut.`type`) }
        }
        val newResolved = resolved ++ Seq(one)
        expand(rest ++ newTypes.filterNot(newResolved.contains), newResolved)
      }
    }
  }

}
