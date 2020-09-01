package apibuilder.rewriter

import graphql.generators.helpers.{ApiBuilderServiceHelpers, MultiServiceHelpers}
import io.apibuilder.spec.v0.models.{Enum, Model, Union}
import io.flow.test.utils.FlowPlaySpec

class MinimalTypesRewriterSpec extends FlowPlaySpec
  with ApiBuilderServiceHelpers
  with MultiServiceHelpers {

  private[this] def rewrite(
    enums: Seq[Enum] = Nil,
    models: Seq[Model] = Nil,
    unions: Seq[Union] = Nil,
    types: Seq[String],
  ) = {
    val ms = makeMultiService(
      makeService(enums = enums, models = models, unions = unions)
    )
    MinimalTypesRewriter(
      ms,
      types.map { t => mustFindType(ms, t) }
    ).rewrite.allTypes.map(_.name).sorted
  }

  "model fields" must {

    "resolve enum type" in {
      rewrite(
        enums = Seq(makeEnum("foo")),
        models = Seq(
          makeModel("user", fields = Seq(makeField(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

    "resolve model type" in {
      rewrite(
        models = Seq(
          makeModel("foo"),
          makeModel("user", fields = Seq(makeField(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

    "resolve union type" in {
      rewrite(
        unions = Seq(makeUnion("foo")),
        models = Seq(
          makeModel("user", fields = Seq(makeField(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

  }

  "unions types" must {

    "resolve enum type" in {
      rewrite(
        enums = Seq(makeEnum("foo")),
        unions = Seq(
          makeUnion("user", types = Seq(makeUnionType(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

    "resolve model type" in {
      rewrite(
        models = Seq(makeModel("foo")),
        unions = Seq(
          makeUnion("user", types = Seq(makeUnionType(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

    "resolve union type" in {
      rewrite(
        unions = Seq(
          makeUnion("foo"),
          makeUnion("user", types = Seq(makeUnionType(`type` = "foo")))
        ),
        types = Seq("user")
      ) must equal(Seq("foo", "user"))
    }

  }

}
