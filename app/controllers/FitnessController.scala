package controllers

import java.time.LocalDateTime

import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc._
import services.{AuthController, AuthService, FitnessRepository}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

@Singleton
class FitnessController @Inject()(cc: ControllerComponents, fr: FitnessRepository, val auth: AuthService)
                                 (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  def insert: Action[AnyContent] = Action.async { implicit req =>
    req.body.asMultipartFormData.flatMap { data =>
        data.file("data").map { file =>
          val json = file.ref.toFile
          val io = Source.fromFile(json)
          val jsonValue = Json.parse(io.getLines().mkString(","))
          io.close()
          jsonValue
        }
    } match {
      case None => Future(message("Can't load data from request"))
      case Some(jsValue) => fr.insertBatch(jsValue) map { res =>
        message(s"Insert ${res.sum} data done.")
      } recover {
        case e:Throwable => message(s"Insert process error. ${e.getMessage}")
      }
    }
  }

  ///////////////////// NOT IOS STAND ////////////////////////

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