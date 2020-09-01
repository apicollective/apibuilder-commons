package apibuilder.rewriter

import graphql.generators.helpers.{ApiBuilderServiceHelpers, MultiServiceHelpers}
import io.apibuilder.spec.v0.models.{Operation, Resource}
import io.flow.test.utils.FlowPlaySpec

class FilterOperationsRewriterSpec extends FlowPlaySpec
  with ApiBuilderServiceHelpers
  with MultiServiceHelpers
{
  private[this] def op(attributeName: Option[String]): Operation = {
    makeOperation(
      attributes = attributeName.toSeq.map { n => makeAttribute(n) },
    )
  }

  // Filters resources based on presence of an attribute
  private[this] def rewrite(resources: Seq[Resource]) = {
    FilterOperationsRewriter(
      makeMultiService(
        makeService(resources = resources)
      )
    ) { op =>
      Some(op).filter(_.attributes.nonEmpty)
    }.rewrite.services().map(_.service).flatMap(_.resources)
  }

  "operations" must {
    "remove resources when all their operations are filtered" in {
      rewrite(
        Seq(
          makeResource(operations = Seq(op(None)))
        )
      ) must be(Nil)
    }

    "keeps resources when all their operations are accepted" in {
      rewrite(
        Seq(
          makeResource(operations = Seq(op(Some(createTestId()))))
        )
      ) match {
        case r :: Nil => {
          r.operations.size must be(1)
        }
        case other => sys.error(s"Expected 1 resource but found ${other.size}")
      }
    }

    "keeps resources when at least one operation has an attribute" in {
      rewrite(
        Seq(
          makeResource(operations = Seq(
            op(None),
            op(Some(createTestId())))
          )
        )
      ) match {
        case r :: Nil => {
          r.operations.size must be(1)
          r.operations.head.attributes.size must be(1)
        }
        case other => sys.error(s"Expected 1 resource but found ${other.size}")
      }
    }

  }
}
