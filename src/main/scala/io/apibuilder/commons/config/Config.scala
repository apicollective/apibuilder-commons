package io.apibuilder.commons.config

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._

case class Config(
  source: ConfigSource,
  profiles: Seq[Profile],
) {
  private val byName = profiles.map { p => p.name -> p }.toMap
  def find(name: String): Option[Profile] = byName.get(name)
}

case class Profile(
  name: String,
  apiUri: String,
  token: Option[String],
)

/**
 * Reads the API Builder Config
 */
object Config {
  object Defaults {
    val Path = "~/.apibuilder/config"
    val ProfileName = "default"
    val ApiUri = "https://api.apibuilder.io"
  }

  def findDefault: ValidatedNec[String, Config] = ConfigBuilder().build()
  def mustFindDefault: Config = validOrErrors(findDefault)

  def mustFindDefaultProfile: Profile = mustFindProfile(
    config = mustFindDefault,
    profileName = Defaults.ProfileName,
  )

  def find(builder: ConfigBuilder, profileName: String): ValidatedNec[String, Profile] = {
    builder.build().andThen { c =>
      find(c, profileName)
    }
  }

  def find(config: Config, profileName: String): ValidatedNec[String, Profile] = {
    config.find(profileName) match {
      case None => errorMsg(
        config.source, s"Cannot find profile with name '$profileName'. Available profiles: ${config.profiles.map(_.name).mkString(", ")}"
      ).invalidNec
      case Some(p) => p.validNec
    }
  }

  def mustFindProfile(builder: ConfigBuilder, profileName: String): Profile = {
    mustFindProfile(
      validOrErrors(builder.build()),
      profileName,
    )
  }

  def mustFindProfile(config: Config, profileName: String): Profile = {
    validOrErrors(
      find(config, profileName)
    )
  }

  private def validOrErrors[T](v: ValidatedNec[String, T]): T = {
    v match {
      case Valid(p) => p
      case Invalid(errors) => sys.error(errors.toNonEmptyList.toList.mkString(", "))
    }
  }

  private def errorMsg(source: ConfigSource, msg: String): String = {
    s"$msg [Config source: ${source.label}]"
  }
}

