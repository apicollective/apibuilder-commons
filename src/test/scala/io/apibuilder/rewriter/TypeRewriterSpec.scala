package apibuilder.rewriter

import apibuilder.validation.Validator
import io.apibuilder.commons.MultiService
import io.apibuilder.spec.v0.models.{Enum, Method, Model, Operation, Resource, Union}
import io.flow.test.utils.FlowPlaySpec

class TypeRewriterSpec extends FlowPlaySpec
  with ApiBuilderServiceHelpers
  with MultiServiceHelpers {

  private[this] def apiBuilderValidator: Validator = init[Validator]

  private[this] def rewriteTypeByName(serviceType: ServiceType, originalName: String, newName: String): Option[ServiceType] = {
    serviceType match {
      case ServiceType.ApiBuilder(typ) if typ.name == originalName => Some(
        ServiceType.ApiBuilder(TypeNameRewriter.changeName(typ, newName))
      )
      case _: ServiceType.ApiBuilder => None
      case _: ServiceType.Scalar => None
    }
  }

  private[this] def rewrite(enums: Seq[Enum] = Nil, models: Seq[Model], unions: Seq[Union] = Nil, resources: Seq[Resource] = Nil)(
    rewriteType: ServiceType => Option[ServiceType]
  ) = {
    val ms = TypeRewriter(
      makeMultiService(
        makeService(enums = enums, models = models, unions = unions, resources = resources)
      )
    )(rewriteType).rewrite
    apiBuilderValidator.mustValidateDefaultProfile(ms)
    ms
  }

  private[this] def firstOperation(ms: MultiService): Operation = {
    ms.services().head.service.resources.head.operations.head
  }

  "rename" must {

    "model fields" in {
      val ms = rewrite(
        models = Seq(
          makeModel("bar"),
          makeModel("foo", fields = Seq(
            makeField(`type` = "long"),
            makeField(`type` = "bar"),
          )
        ),
      )) { t => rewriteTypeByName(t, "bar", "baz") }
      mustFindModel(ms, "foo").model.fields.map(_.`type`) must equal(
        Seq("long", "baz")
      )
    }

    "union types" in {
      val ms = rewrite(
        models = Seq(makeModel("user"), makeModel("bar")),
        unions = Seq(
          makeUnion("foo", types = Seq(
            makeUnionType("user"),
            makeUnionType("bar"),
          )
          ),
      )) { t => rewriteTypeByName(t, "bar", "baz") }
      mustFindUnion(ms, "foo").union.types.map(_.`type`) must equal(
        Seq("user", "baz")
      )
    }

    "operations" must {
      def setup(operation: Operation): Operation = {
        val resourceType = "example_resource"
        val ms = rewrite(
          models = Seq(makeModel(resourceType), makeModel("query")),
          resources = Seq(
            makeResource(
              resourceType,
              operations = Seq(operation),
            )
          )
        ) { t => rewriteTypeByName(t, "query", "foo") }

        findModel(ms, "query") must be(None)
        mustFindModel(ms, "foo")
        firstOperation(ms)
      }

      "body type" in {
        setup(
          makeOperation(Method.Post, body = Some(
            makeBody("query")
          ))
        ).body.map(_.`type`) must equal(
          Some("foo")
        )
      }

      "parameter type" in {
        val resourceType = "example_resource"
        val ms = rewrite(
          enums = Seq(makeValidEnum("status")),
          models = Seq(makeModel(resourceType)),
          resources = Seq(
            makeResource(
              resourceType,
              operations = Seq(
                makeOperation(
                  parameters = Seq(
                    makeParameter(`type` = "status")
                  )
                )
              ),
            )
          )
        ) { t => rewriteTypeByName(t, "status", "string") }

        firstOperation(ms).parameters.map(_.`type`) must equal(
          Seq("string")
        )
      }

      "response type" in {
        setup(
          makeOperation(
            responses = Seq(make200Response("query"))
          )
        ).responses.map(_.`type`) must equal(
          Seq("foo")
        )
      }
    }
  }

  "preserves namespace" in {
    val s1 = makeService(
      namespace = "ns1",
      models = Seq(makeModel("query"))
    )
    val s2 = makeService(
      namespace = "ns2",
      models = Seq(makeModel("foo", fields = Seq(makeField(`type` = "ns1.models.query"))))
    )

    val ms = TypeRewriter(
      makeMultiService(List(s1, s2))
    ) { t => rewriteTypeByName(t, "query", "baz") }.rewrite

    findModel(ms, "query") must be(None)
    mustFindModel(ms, "ns1.models.baz")
    mustFindModel(ms, "ns2.models.foo").model.fields.map(_.`type`) must equal(
      Seq("ns1.models.baz")
    )
  }

  "preserves collection" in {
    val service = makeService(
      models = Seq(
        makeModel("group"),
        makeModel("user", fields = Seq(makeField(`type` = "[group]")))
      )
    )

    val ms = TypeRewriter(
      makeMultiService(service)
    ) { t => rewriteTypeByName(t, "group", "object") }.rewrite

    mustFindModel(ms, "user").model.fields.map(_.`type`) must equal(
      Seq("[object]")
    )
  }

}
