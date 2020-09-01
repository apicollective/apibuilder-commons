package io.apibuilder.commons.validation.helpers

import java.io.File
import io.apibuilder.commons.validation.zip.FileUtil

trait FileHelpers {

  def readFileAsString(file: File): String = FileUtil.readFileAsString(file)

  def writeToTempFile(
    contents: String,
    prefix: String = "apibuildervalidation",
    suffix: String = "tmp"
  ): File = {
    FileUtil.writeToTempFile(
      contents = contents,
      prefix = prefix,
      suffix = suffix
    )
  }
}

