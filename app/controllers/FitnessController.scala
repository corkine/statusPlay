package controllers

import java.time.LocalDateTime

import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc._
import services.{AuthController, AuthService, FitnessRepository}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FitnessController @Inject()(cc: ControllerComponents, fr: FitnessRepository, val auth: AuthService)
                                 (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  def all = Action.async {
    fr.all(LocalDateTime.now().minusDays(3),LocalDateTime.now()).map { req =>
      Ok(Json.toJson(req))
    }
  }

  def schema = Action(Ok(fr.schema))

  def recordDetails(recordId:Long) = Action.async {
    fr.recordDetails(recordId) map {
      case None => message(s"Can't find this record $recordId")
      case Some(value) => Ok(Json.toJson(value))
    }
  }

}