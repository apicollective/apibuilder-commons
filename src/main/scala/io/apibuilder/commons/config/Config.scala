package apibuilder.config

import java.io.File

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._

case class Config(profiles: Seq[Profile]) {
  private[this] val byName = profiles.map { p => p.name -> p }.toMap
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
  val DefaultPath = "~/.apibuilder/config"
  val DefaultProfileName = "default"

  def defaultConfig: ValidatedNec[String, Config] = ConfigBuilder().build()

  def defaultProfile(profileName: String = DefaultProfileName): ValidatedNec[String, Profile] = {
    defaultConfig.andThen { c =>
      c.profiles.find(_.name == profileName) match {
        case None => s"Cannot find profile with name '${profileName}'. Available profiles: ${c.profiles.map(_.name).mkString(", ")}".invalidNec
        case Some(p) => p.validNec
      }
    }
  }

  def mustFindProfile(profileName: String = DefaultProfileName): Profile = {
    defaultProfile(profileName = profileName) match {
      case Invalid(errors) => sys.error(errors.toNonEmptyList.toList.mkString(", "))
      case Valid(p) => p
    }
  }
}

case class ConfigBuilder(
  path: String = Config.DefaultPath,
  profileName: String = Config.DefaultProfileName,
) {

  def withPath(path: String): ConfigBuilder = {
    this.copy(path = path)
  }

  def withProfileName(profileName: String): ConfigBuilder = {
    this.copy(profileName = profileName)
  }

  def build(): ValidatedNec[String, Config] = {
    val file = new File(expandPath(path))
    if (file.isFile) {
      ConfigParser.parse(
        scala.io.Source.fromFile(file).mkString
      )
    } else {
      s"ApiBuilder Config File '$path' was not found or could not be read".invalidNec
    }
  }

  private[this] def expandPath(path: String): String = {
    if (path.startsWith("~" + File.separator)) {
      System.getProperty("user.home") + path.substring(1)
    } else {
      path
    }
  }

}
