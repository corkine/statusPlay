package controllers

import javax.inject.{Inject, Singleton}
import play.api.cache.AsyncCacheApi
import play.api.libs.json.Json
import play.api.mvc._
import services.{AuthController, AuthService, FitnessRepository}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FitnessController @Inject()(cc: ControllerComponents, fr: FitnessRepository,
                                  val cache: AsyncCacheApi,
                                  val auth: AuthService)
                                 (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  def all(category:Option[String],lastDays:Option[Int],
          durationBiggerSeconds:Option[Long],
          skip:Option[Long],limit:Option[Long]): Action[AnyContent] = Action.async { implicit req =>
    authUsers(req) flatMap {
      case Right(v) => Future(v)
      case Left(_) => fr.all(category,lastDays,durationBiggerSeconds,skip,limit) map { res =>
        Ok(Json.toJson(res))
      }
    }
  }

  def detailsRecord(recordId:Long): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => fr.details(recordId) map {
        case None => message(s"can't find record for $recordId")
        case Some(value) => Ok(Json.toJson(value))
      }
    }
  }

  def deleteRecord(recordId:Long): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => fr.delete(recordId) map message
    }
  }

  def insert: Action[AnyContent] = Action.async { implicit req =>
    req.body.asFormUrlEncoded.flatMap { data =>
      (for {
        u <- data.get("user").map(_.head)
        p <- data.get("password").map(_.head)
        d <- data.get("data").map { case List(a,_*) => Json.parse(a) }
      } yield (u,p,d)).map { case (u,p,d) =>
        auth.check(u,p) map {
          case None => Left("Auth Failed.")
          case Some(_) => Right(d)
        }
      } //Option[Future[Either[String,User]]]
    } match {
      case None => Future(message("Can't load data, user or password from request"))
      case Some(f) => f.flatMap {
        case Left(value) => Future(message(value))
        case Right(jsValue) => fr.insertBatch2(jsValue) map { res =>
          message(s"Insert ${res.sum} data done.")
        }
      } recover {
        case e:Throwable => message(s"Insert process error. ${e.getMessage}")
      }
    }
  }

  def schema = Action(Ok(fr.schema))

}