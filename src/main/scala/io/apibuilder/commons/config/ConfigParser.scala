package apibuilder.config

import cats.data.ValidatedNec
import cats.implicits._

object ConfigParser {

  val DefaultApiUri = "https://api.apibuilder.io"

  def parse(value: String): ValidatedNec[String, Config] = {
    ConfigParser().parse(value).toList match {
      case Nil => "No Api Builder Profiles found".invalidNec
      case profiles => Config(profiles).validNec
    }
  }
}

/**
 * Reads the API Builder Config
 */
case class ConfigParser() {

  private[this] val profiles = scala.collection.mutable.ListBuffer[Profile]()

  private[this] var captureName = false
  private[this] var captureKey = false
  private[this] var captureValue = false

  private[this] val keyValues = scala.collection.mutable.Map[String, String]()
  private[this] var name = ""
  private[this] var key = ""
  private[this] var value = ""

  private[this] def reset(): Unit = {
    keyValues.clear()
    clearCaptures()
    name = ""
    key = ""
    value = ""
  }

  private[this] def clearCaptures(): Unit = {
    captureName = false
    captureKey = false
    captureValue = false
  }

  private[this] def saveProfile(): Unit = {
    if (name.nonEmpty) {
      profiles.append(
        Profile(
          name = name,
          apiUri = keyValues.getOrElse("apiUri", ConfigParser.DefaultApiUri),
          token = keyValues.get("token"),
        )
      )
    }
  }

  def parse(contents: String): Seq[Profile] = {
    contents.foreach { c =>
      if (c == '[') {
        saveProfile()
        reset()
        clearCaptures()
        captureName = true
      } else if (c == ']') {
        clearCaptures()
        captureKey = true
      } else if (c == '=') {
        clearCaptures()
        captureValue = true
      } else if (c == '\n') {
        clearCaptures()
        captureKey = true
      } else if (captureName) {
        name += c
      } else if (captureKey) {
        key += c
      } else if (captureValue) {
        value += c
      } else {
        sys.error(s"Unexpected character '$c' while parsing config")
      }
    }
    saveProfile()
    profiles.toSeq
  }

}


