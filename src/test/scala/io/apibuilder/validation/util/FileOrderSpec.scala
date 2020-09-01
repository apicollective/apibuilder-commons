package io.apibuilder.commons.validation.util

import io.apibuilder.commons.validation.helpers
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec

class FileOrderSpec extends AnyFunSpec with Matchers
  with helpers.FileHelpers
{
  it("sortOrder with no ordering file is alphabetical") {
    FileOrder(None).sort(Seq("b", "a")) should equal(
      List("a", "b")
    )
  }

  it("sortOrder respects ordering file") {
    FileOrder(
      Some(writeToTempFile(contents = "b\na"))
    ).sort(Seq("b", "a")) should equal(
      List("b", "a")
    )
  }

  it("sortOrder respects ordering file for entries in it, alphabetical for rest") {
    FileOrder(
      Some(writeToTempFile(contents = "c"))
    ).sort(Seq("c", "b", "a")) should equal(
      List("c", "a", "b")
    )
  }

}