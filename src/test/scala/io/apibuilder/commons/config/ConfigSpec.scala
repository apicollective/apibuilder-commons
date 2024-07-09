package io.apibuilder.commons.config

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConfigSpec extends AnyWordSpec with Matchers {

  private def assertInvalid[T](value: ValidatedNec[String, T]): Seq[String] = {
    value match {
      case Valid(_) => sys.error("Expected invalid")
      case Invalid(errors) => errors.toNonEmptyList.toList
    }
  }

  private def assertValid[T](value: ValidatedNec[String, T]): T = {
    value match {
      case Valid(r) => r
      case Invalid(errors) => sys.error("Unexpected errors: " + errors.toNonEmptyList.toList.mkString(", "))
    }
  }

  "find validation" must {

    def error(msg: String): String = s"$msg [Config source: Explicitly Provided]"

    "ensure at least 1 profile" in {
      val builder = ConfigBuilder().withContents("")
      assertInvalid(
        Config.find(builder, "foo")
      ) must equal(
        Seq(error("No Api Builder Profiles found"))
      )
    }

    "check profile name" in {
      val builder = ConfigBuilder().withContents("[profile foo]")
      assertInvalid(
        Config.find(builder, "bar")
      ) must equal(
        Seq(error("Cannot find profile with name 'bar'. Available profiles: foo"))
      )
    }

    "check for duplicate names" in {
      assertInvalid(
        ConfigBuilder().withContents("[profile foo]\n[profile foo]").build()
      ) must equal(
        Seq(error("Profile names must be unique. Found duplicates: foo"))
      )
    }
  }

  "find parser" must {

    "parse profile names" in {
      val builder = ConfigBuilder().withContents("[profile foo]")
      assertValid(
        Config.find(builder, "foo")
      ).name must equal("foo")
    }

    "parse default profile" in {
      val builder = ConfigBuilder().withContents("[default]")
      assertValid(
        Config.find(builder, "default")
      ).name must equal("default")
    }

    "parse multiple profiles" in {
      val builder = ConfigBuilder().withContents("[default]\n[profile foo]")
      assertValid(builder.build()).profiles.map(_.name) must equal(
        Seq("default", "foo")
      )
    }

    "parse token" must {
      def setup(value: String) = {
        val builder = ConfigBuilder().withContents(s"[profile foo]\n$value")
        assertValid(
          Config.find(builder, "foo")
        ).token
      }

      "ignore whitespace" in {
        setup(" token  = t ") must equal(Some("t"))
      }

      "assign token" in {
        setup("token=value") must equal(Some("value"))
      }
    }

    "parse apiUrl" must {
      def setup(uri: Option[String]) = {
        val value = uri match {
          case None => ""
          case Some(u) => s"apiUri=$u"
        }
        Config.mustFindProfile(
          ConfigBuilder().withContents(s"[default]\n$value"),
          "default",
        ).apiUri
      }

      "default to constant" in {
        setup(None) must equal(Config.Defaults.ApiUri)
      }

      "assign apiUri" in {
        val uri = "https://test.com"
        setup(Some(uri)) must equal(uri)
      }

      "ignore whitespace" in {
        val uri = "https://test.com"
        setup(Some(s"   $uri   ")) must equal(uri)
      }
    }
  }

}
