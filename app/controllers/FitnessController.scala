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

  def all(category:Option[String],lastDays:Option[Int],
          durationBiggerSeconds:Option[Long],
          skip:Option[Long],limit:Option[Long]): Action[AnyContent] = Action.async { implicit req =>
    fr.all(category,lastDays,durationBiggerSeconds,skip,limit) map { res =>
      Ok(Json.toJson(res))
    }
  }

  def insert: Action[AnyContent] = Action.async { implicit req =>
    req.body.asFormUrlEncoded.flatMap { data =>
      data.get("data").map { case List(a,_*) => Json.parse(a) }
    } match {
      case None => Future(message("Can't load data from request"))
      case Some(jsValue) => fr.insertBatch2(jsValue) map { res =>
        message(s"Insert ${res.sum} data done.")
      } recover {
        case e:Throwable => message(s"Insert process error. ${e.getMessage}")
      }
    }
  }

  ///////////////////// NOT IOS STAND ////////////////////////

  def allRecord = Action.async {
    fr.allRecord(LocalDateTime.now().minusDays(3),LocalDateTime.now()).map { req =>
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