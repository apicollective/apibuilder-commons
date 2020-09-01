package apibuilder.rewriter

import graphql.generators.helpers.{ApiBuilderServiceHelpers, MultiServiceHelpers}
import io.apibuilder.spec.v0.models.{Model, Union}
import io.flow.test.utils.FlowPlaySpec

class UnionsToModelsRewriterSpec extends FlowPlaySpec
  with ApiBuilderServiceHelpers
  with MultiServiceHelpers {

  private[this] def rewrite(models: Seq[Model], unions: Seq[Union]) = {
    UnionsToModelsRewriter(
      makeMultiService(
        makeService(models = models, unions = unions)
      )
    ).rewrite
  }

  private[this] def union(name: String, discrimninator: String, types: Seq[String]): Union = {
    makeUnion(
      name,
      types = types.map { t => makeUnionType(t) },
      discriminator = Some(discrimninator),
    )
  }

  "rewrites union to a model and enum" in {
    val models = Seq(makeModel("user"), makeModel("group"))
    val ms = rewrite(
      models = models,
      unions = Seq(union("party", "disc", models.map(_.name))),
    )

    findUnion(ms, "party") must be(None)
    models.forall { m => findModel(ms, m.name).isDefined } must be(true)

    val party = mustFindModel(ms, "party").model
    party.fields.map(_.name) must equal(Seq("disc"))
    mustFindEnum(ms, "party_disc").`enum`.values.map(_.name) must equal(
      Seq("user", "group")
    )
  }
}
