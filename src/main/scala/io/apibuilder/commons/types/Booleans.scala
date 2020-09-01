package io.apibuilder.commons.types

/**
 * We auto convert strings to booleans based on common, well known
 * values from various frameworks. For example, the string 'true' or
 * 't' will result in the boolean true - see TrueValues and
 * FalseValues for all supported strings.
 */
object Booleans {

  val TrueValues: Seq[String] = Seq("t", "true", "y", "yes", "on", "1", "trueclass")
  val FalseValues: Seq[String] = Seq("f", "false", "n", "no", "off", "0", "falseclass")

}
