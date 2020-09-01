package io.apibuilder.commons.validation

import akka.actor.ActorSystem
import akka.stream.{Materializer, SystemMaterializer}
import io.apibuilder.commons.config.Profile
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import play.api.libs.json.{JsArray, JsBoolean, Json}
import io.apibuilder.spec.v0.models.Service
import io.apibuilder.spec.v0.models.json._
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import play.api.libs.ws.WSAuthScheme
import play.shaded.ahc.org.asynchttpclient.{DefaultAsyncHttpClient, DefaultAsyncHttpClientConfig}
import play.api.libs.ws.DefaultBodyWritables._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

object Validator {

  def default(): Validator = {
    Validator(defaultClient())
  }

  def defaultClient(materializer: Materializer = defaultMaterializer()): StandaloneAhcWSClient = {
    new StandaloneAhcWSClient(
      new DefaultAsyncHttpClient(
        new DefaultAsyncHttpClientConfig.Builder()
          .setMaxRequestRetry(0)
          .setShutdownQuietPeriod(0)
          .setShutdownTimeout(0).build
      )
    )(materializer)
  }

  def defaultMaterializer(): Materializer = {
    // Create Akka system for thread and streaming management
    val system = ActorSystem()
    system.registerOnTermination {
      System.exit(0)
    }
    SystemMaterializer(system).materializer
  }
}

/**
 * Given a service, calls the Api Builder Validations resource to check if the
 * specification of the service is valid.
 */
case class Validator(wsClient: StandaloneAhcWSClient) {

  def mustValidate(profile: Profile, service: Service)(
    implicit ec: ExecutionContext
  ): Future[Unit] = {
    validate(profile, service).map {
      case Valid(_) => ()
      case Invalid(errors) => sys.error("Validation Failed:" + errors.toNonEmptyList.toList.mkString("\n -)"))
    }
  }

  def validate(profile: Profile, service: Service)(
    implicit ec: ExecutionContext
  ): Future[ValidatedNec[String, Unit]] = {
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
        case 200 | 422 => parseResult(r.body)
        case n => {
          s"API Call failed with unexpected status code: $n".invalidNec
        }
      }
    }
  }

  private[this] def parseResult(body: String): ValidatedNec[String, Unit] = {
    val js = Json.parse(body)
    if ((js \ "valid").asOpt[JsBoolean].exists(_.value)) {
      ().validNec
    } else {
      (js \ "errors").as[JsArray].value.map(_.toString).mkString(", ").invalidNec
    }
  }

}
