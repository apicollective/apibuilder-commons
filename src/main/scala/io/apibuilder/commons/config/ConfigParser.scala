package io.apibuilder.commons.config

import cats.data.ValidatedNec
import cats.implicits._

private[config] object ConfigParser {

  def parse(source: ConfigSource, value: String): ValidatedNec[String, Config] = {
    ConfigParser().parse(value).toList match {
      case Nil => errorMsg(source, "No Api Builder Profiles found").invalidNec
      case profiles => validateProfiles(source, profiles).map { p =>
        Config(source, p)
      }
    }
  }

  private[this] def validateProfiles(source: ConfigSource, profiles: List[Profile]): ValidatedNec[String, List[Profile]] = {
    profiles.groupBy(_.name).filter(_._2.size > 1).keys.toList match {
      case Nil => profiles.validNec
      case multiple => errorMsg(source, s"Profile names must be unique. Found duplicates: ${multiple.mkString(", ")}").invalidNec
    }
  }

  private[this] def errorMsg(source: ConfigSource, value: String): String = {
    s"$value [Config source: ${source.label}]"
  }

}

/**
 * Reads the API Builder Config
 */
private[config] case class ConfigParser() {

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
          name = stripProfilePrefix(name),
          apiUri = keyValues.getOrElse("apiUri", Config.Defaults.ApiUri),
          token = keyValues.get("token"),
        )
      )
    }
  }

  private[this] val Prefix = "profile"
  private[this] def stripProfilePrefix(name: String): String = {
    if (name.startsWith(Prefix)) {
      name.drop(Prefix.length).trim
    } else {
      name
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
        if (key.trim.nonEmpty && value.trim.nonEmpty) {
          keyValues.addOne(key.trim -> value.trim)
        }
      } else {
        sys.error(s"Unexpected character '$c' while parsing config")
      }
    }
    saveProfile()
    profiles.toSeq
  }

}


