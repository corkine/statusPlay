package controllers

import java.time.LocalDateTime

import javax.inject.{Inject, Singleton}
import play.api.cache.AsyncCacheApi
import play.api.libs.json.Json
import play.api.mvc._
import services.{ActivityRepository, AuthController, AuthService, Website}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ActivityController @Inject()(cc: ControllerComponents, ar: ActivityRepository,
                                   val cache: AsyncCacheApi,
                                   val auth: AuthService)
                                  (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  def schema: Action[AnyContent] = Action.async { req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => Future(Ok(ar.schema))
    }
  }

  def addWebsite(name:String,url:String,note:Option[String],priority:Option[Int]): Action[AnyContent] = Action.async { req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) =>
        val formatUrl = if (url.startsWith("http://") || url.startsWith("https://")) url else "http://" + url
        ar.addWebsite(name,formatUrl,note,priority.getOrElse(1)) map {
          case 0 => message(s"Add failed, exist url $formatUrl in database.")
          case o => message(s"Add done with $formatUrl, insert $o")
        }
    }
  }

  def updateWebsite(id:Long,name:Option[String],url:Option[String],
                    note:Option[String],priority:Option[Int]): Action[AnyContent] = Action.async { req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) =>ar.updateWebsite(id,name,url,note,priority) map {
        case 0 => message("update failed.")
        case o => message(s"update done with $o affect.")
      }
    }
  }

  def deleteWebsite(websiteId:Long): Action[AnyContent] = Action.async { req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => ar.deleteWebsite(websiteId) map {
        case 0 => message("Delete failed, can't find Id")
        case o => message(s"Delete done with id $websiteId, changed $o row(s).")
      }
    }
  }

  def checkActivities(websiteId:Long,skip:Option[Int],limit:Option[Int]): Action[AnyContent] = Action.async { req =>
    authUsers(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => ar.websiteActivitiesWithInfoByAction(websiteId,skip,limit) map {
        case Left(value) => Ok(Json.obj("message" -> value))
        case Right(value) => Ok(Json.obj("website" -> value._1, "activities" -> value._2))
      }
    }
  }

  def all(lastDay:Option[Long]): Action[AnyContent] = Action.async {
    val fromDate = LocalDateTime.now().minusDays(lastDay.getOrElse(3))
    val endDate = LocalDateTime.now()
    ar.websitesInfo(fromDate,endDate) map { r =>
      Ok(Json.toJson(r.map { e => Json.obj(
        "name" -> e._1,
        "websiteUrl" -> e._2,
        "websiteNote" -> e._3,
        "websiteId" -> e._4,
        "healthy" -> e._5,
        "activityCount" -> e._6,
        "from" -> fromDate,
        "end" -> endDate
      )}))
    }
  }
}
