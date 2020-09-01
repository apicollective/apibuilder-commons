package io.apibuilder.commons.validation

import java.security.Provider.Service

import io.apibuilder.commons.config.{Config, Profile}
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import play.api.libs.json.{JsArray, JsBoolean, JsValue, Json}
import io.apibuilder.spec.v0.models.json._
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import play.api.libs.ws.WSAuthScheme
import play.shaded.ahc.org.asynchttpclient.{DefaultAsyncHttpClient, DefaultAsyncHttpClientConfig}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Validator {

  def default(): Validator = {
    Validator(defaultClient())
  }

  def defaultClient(): StandaloneAhcWSClient = {
    val asyncHttpClientConfig = new DefaultAsyncHttpClientConfig.Builder()
      .setMaxRequestRetry(0)
      .setShutdownQuietPeriod(0)
      .setShutdownTimeout(0).build
    val asyncHttpClient = new DefaultAsyncHttpClient(asyncHttpClientConfig)
    new StandaloneAhcWSClient(asyncHttpClient)
  }
}

/**
 * Given a service, calls the Api Builder Validations resource to check if the
 * specification of the service is valid.
 */
case class Validator(wsClient: StandaloneAhcWSClient) {

  def mustValidate(profile: Profile, service: Service): Unit = {
    validate(profile, service).map {
      case Valid(_) => ()
      case Invalid(errors) => sys.error("Validation Failed:" + errors.toNonEmptyList.toList.mkString("\n -)"))
    }
  }

  def validate(profile: Profile, service: Service): Future[ValidatedNec[String, Unit]] = {
    val url = profile.apiUri + "/validations"
    val js = Json.prettyPrint(Json.toJson(service))
    val request = wsClient
      .url(url)
      .addHttpHeaders(
        "Content-type" -> "application/json",
      )
      .withRequestTimeout(10.seconds)

    profile.token.foldLeft(request) { case (r, t) =>
      r.withAuth("", t, WSAuthScheme.BASIC)
    }.post(js).map { r =>
      r.status match {
        case 200 | 422 => parseResult(js, r.body)
        case n => {
          s"API Call failed with unexpected status code: $n".invalidNec
        }
      }
    }
  }

  private[this] def parseResult(requestBody: String, body: String): ValidatedNec[String, Unit] = {
    val js = Json.parse(body)
    if ((js \ "valid").asOpt[JsBoolean].map(_.value).getOrElse(false)) {
      ().validNec
    } else {
      (js \ "errors").as[JsArray].value.map(_.toString).mkString(", ").invalidNec
    }
  }

}
