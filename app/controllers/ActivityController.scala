package controllers

import java.time.LocalDateTime

import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc._
import services.{ActivityRepository, AuthController, AuthService}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ActivityController @Inject()(cc: ControllerComponents, ar: ActivityRepository, val auth: AuthService)
                                  (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  def schema: Action[AnyContent] = Action.async { req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => Future(Ok(ar.schema))
    }
  }

  def checkActivities(websiteId:Long): Action[AnyContent] = Action.async { req =>
    authUsers(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => ar.websiteActivitiesWithInfoByAction(websiteId) map {
        case Left(value) => Ok(Json.obj("message" -> value))
        case Right(value) => Ok(Json.obj("website" -> value._1, "activities" -> value._2))
      }
    }
  }

  def all: Action[AnyContent] = Action.async {
    ar.websitesInfo(LocalDateTime.now().minusDays(3),LocalDateTime.now()) map { r =>
      Ok(Json.toJson(r.map { e => Json.obj(
        "name" -> e._1,
        "websiteUrl" -> e._2,
        "healthy" -> e._4,
        "activityCount" -> e._5
      )}))
    }
  }
}
