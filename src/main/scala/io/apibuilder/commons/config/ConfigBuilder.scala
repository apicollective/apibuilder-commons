package io.apibuilder.commons.config

import java.io.File
import cats.data.ValidatedNec
import cats.implicits._

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
      ConfigParser.parse(c)
    }
  }

  private[this] def validateContents(): ValidatedNec[String, String] = {
    (path, contents) match {
      case (Some(_), Some(_)) => "Cannot specify both a config path and config contents".invalidNec
      case (Some(p), None) => validatePath(p)
      case (None, Some(c)) => c.validNec
      case (None, None) => validatePath(Config.Defaults.Path)
    }
  }

  private[this] def validatePath(path: String): ValidatedNec[String, String] = {
    val file = new File(expandPath(path))
    if (file.isFile) {
      scala.io.Source.fromFile(file).mkString.validNec
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


