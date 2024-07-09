package io.apibuilder.commons.config

import java.io.File
import cats.data.ValidatedNec
import cats.implicits._

sealed trait ConfigSource {
  def label: String
}
object ConfigSource {
  case object Provided extends ConfigSource {
    override def label = "Explicitly Provided"
  }
  case class File(path: String) extends ConfigSource {
    override def label = s"File '$path'"
  }
}

// Keeps track of from where we loaded the config to assist debugging
// if there is an error
private[config] case class ConfigSourceWrapper(source: ConfigSource, contents: String)

case class ConfigBuilder(
  path: Option[String] = None,
  contents: Option[String] = None,
) {

  def withPath(path: String): ConfigBuilder = {
    this.copy(path = Some(path))
  }

  def withContents(contents: String): ConfigBuilder = {
    this.copy(contents = Some(contents))
  }

  def build(): ValidatedNec[String, Config] = {
    validateContents().andThen { c =>
      ConfigParser.parse(c.source, c.contents)
    }
  }

  private def validateContents(): ValidatedNec[String, ConfigSourceWrapper] = {
    (path, contents) match {
      case (Some(_), Some(_)) => "Cannot specify both a config path and config contents".invalidNec
      case (Some(p), None) => validatePath(p)
      case (None, Some(c)) => ConfigSourceWrapper(
        source = ConfigSource.Provided,
        contents = c,
      ).validNec
      case (None, None) => validatePath(Config.Defaults.Path)
    }
  }

  private def validatePath(path: String): ValidatedNec[String, ConfigSourceWrapper] = {
    val file = new File(expandPath(path))
    if (file.isFile) {
      val source = scala.io.Source.fromFile(file)
      try
        ConfigSourceWrapper(
          source = ConfigSource.File(file.getAbsolutePath),
          contents = source.mkString
        ).validNec
      finally {
        source.close()
      }
    } else {
      s"ApiBuilder Config File '$path' was not found or could not be read".invalidNec
    }
  }

  private def expandPath(path: String): String = {
    if (path.startsWith("~" + File.separator)) {
      System.getProperty("user.home") + path.substring(1)
    } else {
      path
    }
  }

}


