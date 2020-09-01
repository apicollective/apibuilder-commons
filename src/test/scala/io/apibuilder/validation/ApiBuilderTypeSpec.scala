package io.apibuilder.commons.validation

import io.apibuilder.spec.v0.models.{Enum, Model, Union}
import io.apibuilder.commons.validation.helpers.Helpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class ApiBuilderTypeSpec extends AnyFunSpec with Matchers with Helpers {

  private[this] lazy val service = loadService("flow-api-service.json").service
  private[this] lazy val enum = ApiBuilderType.Enum(service, Enum("gender", "genders", values = Nil))
  private[this] lazy val model = ApiBuilderType.Model(service, Model("user", "users", fields = Nil))
  private[this] lazy val union = ApiBuilderType.Union(service, Union("test", "tests", types = Nil))

  it("typeName") {
    enum.typeName should equal(TypeName("gender", service.namespace))
    model.typeName should equal(TypeName("user", service.namespace))
    union.typeName should equal(TypeName("test", service.namespace))
  }

  it("namespace") {
    enum.namespace should equal(service.namespace)
    model.namespace should equal(service.namespace)
    union.namespace should equal(service.namespace)
  }

  it("name") {
    enum.name should equal("gender")
    model.name should equal("user")
    union.name should equal("test")
  }

  it("qualified") {
    enum.qualified should equal(service.namespace + ".enums.gender")
    model.qualified should equal(service.namespace + ".models.user")
    union.qualified should equal(service.namespace + ".unions.test")
  }

  it("qualified is compatible with Type Name") {
    TypeName.parse(defaultNamespace = "foo", name = enum.qualified) should equal(
      TypeName(name = "gender", namespace = service.namespace)
    )
  }

}
