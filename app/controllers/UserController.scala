package controllers

import javax.inject.{Inject, Singleton}
import play.api.cache.AsyncCacheApi
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import services.AuthBasic.{Admin, Common, User, UserType}
import services.{AuthController, AuthService}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UserController @Inject()(cc: ControllerComponents,
                                   val cache: AsyncCacheApi,
                                   val auth: AuthService)
                                  (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  def deleteUser(id:Long): Action[AnyContent] = Action.async { req =>
    authAdmin(req) flatMap {
      case Left(_) => auth.delete(id) map {
        case 1 => message("Delete 1 row done.")
        case o => message(s"Error, delete $o rows.")
      }
      case Right(value) => Future(value)
    }
  }

  def list: Action[AnyContent] = Action.async { req =>
    authAdmin(req) flatMap {
      case Right(value) => Future(value)
      case Left(_) => auth.list map { us =>
        Ok(Json.toJson(us))
      }
    }
  }

  def addUser(uName:String,uPassword:String,uRole:Option[String]): Action[AnyContent] = Action.async { req =>
    authAdmin(req) flatMap {
      case Right(v) => Future(v)
      case Left(_) =>
        val types = uRole.flatMap(_.toUpperCase match {
          case "ADMIN" => Some(Admin)
          case "COMMON" => Some(Common)
          case _ => None
        }).getOrElse(Common)
        auth.add(uName, uPassword, types) map {
          case true => message("添加成功.")
          case false => message("添加失败.")
        }
    }
  }

}
