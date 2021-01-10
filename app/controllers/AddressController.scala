package controllers

import javax.inject.{Inject, Singleton}
import org.slf4j.LoggerFactory
import play.api.cache.AsyncCacheApi
import play.api.libs.json.Json
import play.api.mvc._
import services.{Address, AddressRepository, AuthController, AuthService, FitnessRepository}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AddressController @Inject()(cc: ControllerComponents, ar: AddressRepository,
                                  val cache: AsyncCacheApi,
                                  val auth: AuthService)
                                 (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  def all(lastDays:Option[Int],
          recentFirst:Option[Boolean],
          skip:Option[Long],limit:Option[Long]): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(v) => Future(v)
      case Left(_) => ar.allAddresses(lastDays,recentFirst.getOrElse(true),skip.getOrElse(0),limit.getOrElse(500)) map { res =>
        Ok(Json.toJson(res))
      }
    }
  }

  def searchHost(hostName:String, take:Option[Long], fullMatch:Option[Boolean], days:Option[Long]): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => ar.findHostNameIn(hostName, take, fullMatch.getOrElse(true), days).map { res =>
        Ok(Json.toJson(res))
      }
    }
  }

  def one(hostName:String): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => ar.findHostNameIn(hostName, Some(1), fullMatch = true, None).map { res =>
        Ok(Json.toJson(res.headOption))
      }
    }
  }

  def searchIP(ip:String, days:Option[Long]): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => ar.findUsedIPIn(ip,days).map { res =>
        Ok(Json.toJson(res))
      }
    }
  }

  def deleteHost(hostName:String): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => ar.deleteAllOfHostName(hostName).map {
        case Left(e) => Ok(Json.obj("status" -> 0, "message" -> e))
        case Right(m) => Ok(Json.obj("status" -> 1, "message" -> m))
      }
    }
  }

  def add(hostName:String, ip:String, description:Option[String]): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) =>
        ar.addRecord(Address(hostName,ip,description)) map {
        case Left(value) => Ok(Json.obj("status" -> 0, "message" -> value))
        case Right(a) => Ok(Json.obj("status" -> 1, "record" -> a))
      }
    }
  }

  def addPost(): Action[AnyContent] = Action.async { implicit req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) =>
        req.body.asFormUrlEncoded.flatMap { data =>
          (data.get("hostName").map(_.head), data.get("ip").map(_.head)) match {
            case (Some(a),Some(b))  => Some(Address(a,b,data.get("description").map(_.head)))
            case _ => None
          }
        }  match {
          case None => Future(Ok(Json.obj("status" -> 0, "message" -> "Pass hostName, ip and may description here.")))
          case Some(address) => ar.addRecord(address) map {
            case Left(value) => Ok(Json.obj("status" -> 0, "message" -> value))
            case Right(a) => Ok(Json.obj("status" -> 1, "record" -> a))
          }
        }
    }
  }

  def schema: Action[AnyContent] = Action(Ok(ar.schema))

}