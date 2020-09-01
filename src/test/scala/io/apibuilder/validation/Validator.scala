package apibuilder.validation

import io.apibuilder.commons.config.{Config, Profile}
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits._
import io.apibuilder.commons.MultiService
import play.api.libs.json.{JsArray, JsValue, Json}
import io.apibuilder.spec.v0.models.json._
import io.flow.event.JsonUtil
import javax.inject.Inject
import play.api.libs.ws.{WSAuthScheme, WSClient}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/**
 * Given a service, calls the Api Builder Validations resource to check if the
 * specification of the service is valid. We use this to test our transformations
 * on the services to ensure that when we rewrite a service it continues to be
 * valid.
 */
class Validator @Inject() (
  wsClient: WSClient,
) {
  private[this] implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  def mustValidateDefaultProfile(multiService: MultiService): Unit = {
    mustValidate(Config.mustFindDefaultProfile, multiService)
  }

  def mustValidate(profile: Profile, multiService: MultiService): Unit = {
    await(validate(profile, multiService)) match {
      case Valid(_) => ()
      case Invalid(errors) => {
        sys.error("Validation Failed:" + errors.toNonEmptyList.toList.mkString("\n -)"))
      }
    }
  }

  def validate(profile: Profile, multiService: MultiService): Future[ValidatedNec[String, Unit]] = {
    val url = profile.apiUri + "/validations"
    val js = Json.prettyPrint(toJson(multiService))
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
    if (JsonUtil.requiredBoolean(js, "valid")) {
      // println(s"ApiBuilder Validation Succeeded for body:\n$requestBody\n\n")
      ().validNec
    } else {
      println(s"ApiBuilder Validation Failed for body:\n$requestBody\n\n")
      (js \ "errors").as[JsArray].value.map(_.toString).mkString(", ").invalidNec
    }
  }

  private[this] def await[T](f: Future[T]): T = {
    Await.result(f, FiniteDuration(10, SECONDS))
  }

  private[this] def toJson(multiService: MultiService): JsValue = {
    multiService.services() match {
      case Nil => Json.obj()
      case one :: Nil => Json.toJson(one.service)
      case _ => sys.error("Cannot serialize multiple services")
    }

  }
}
