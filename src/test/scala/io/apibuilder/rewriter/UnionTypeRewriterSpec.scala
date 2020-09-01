package apibuilder.rewriter

import io.apibuilder.spec.v0.models.{Service, UnionType}
import io.apibuilder.commons.types.ApiBuilderService
import io.flow.test.utils.FlowPlaySpec

class UnionTypeRewriterSpec extends FlowPlaySpec
  with ApiBuilderServiceHelpers
  with MultiServiceHelpers
{

  private[this] def rewrite(service: Service)(
    rewriteTypes: ApiBuilderType.Union => Seq[UnionType]
  ): ApiBuilderService = {
    val ms = makeMultiService(ApiBuilderService(service))
    UnionTypeRewriter(ms)(rewriteTypes).rewrite.services().head
  }

  "recursively cleans up empty unions" in {
    // expect union to be removed as once we remove the enum it is empty. Then expect
    // the model field type to change from the union type to object
    val group = makeEnum("group")
    val union = makeUnion("union", types = Seq(makeUnionType(group.name)))
    val person = makeModel("person", fields = Seq(
      makeField("id"), makeField(`type` = union.name)
    ))

    val service = rewrite(
      makeService(
        enums = Seq(group),
        models = Seq(person),
        unions = Seq(union),
      )
    )(_ => Nil)

    findUnion(service, union.name) must be(None)
    mustFindModel(service, person.name).model.fields.map(_.`type`) must equal(Seq("string", "object"))
  }

  "filters union types to the reduced set" in {
    def setup(name: String) = {
      (makeModel(name), makeUnionType(name))
    }
    val (modelA, unionTypeA) = setup("a")
    val (modelB, unionTypeB) = setup("b")
    val union = makeUnion("union", types = Seq(unionTypeA, unionTypeB))

    val service = rewrite(
      makeService(
        models = Seq(modelA, modelB),
        unions = Seq(union),
      )
    )(_ => Seq(unionTypeA))

    mustFindUnion(service, union.name).union.types.map(_.`type`) must equal(Seq("a"))
  }
}
