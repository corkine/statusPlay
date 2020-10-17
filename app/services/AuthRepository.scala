package services

import java.util.Base64

import play.api.libs.functional.syntax._
import javax.inject.{Inject, Singleton}
import play.api.cache.AsyncCacheApi
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.http.HeaderNames
import play.api.libs.functional.syntax.unlift
import play.api.libs.json.{Format, JsError, JsPath, JsResult, JsString, JsSuccess, JsValue, Json}
import play.api.mvc.{AbstractController, AnyContent, QueryStringBindable, Request, Result}
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

object AuthBasic {

  case class User(username:String, password:String, userType: UserType = Common, id:Long = 0L)

  trait UserType
  object UserType {
    implicit def pathBinder: QueryStringBindable[UserType] = new QueryStringBindable[UserType] {
      override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, UserType]] = {
        params.get("userType").map(i => i.head.toUpperCase() match {
          case "ADMIN" => Right(Admin)
          case "COMMON" => Right(Common)
          case _ => Left(s"Can't parse $key - $i to UserType.")
        })
      }
      override def unbind(key: String, value: UserType): String = {
        value match {
          case Admin => "Admin"
          case Common => "Common"
        }
      }
    }
  }
  object Admin extends UserType
  object Common extends UserType

  object User {
    implicit val utFormats: Format[UserType] = new Format[UserType] {
      override def reads(json: JsValue): JsResult[UserType] = json.as[String].toUpperCase match {
        case "ADMIN" => JsSuccess(Admin)
        case "COMMON" => JsSuccess(Common)
        case _ => JsError("Can't read from userType Json Type.")
      }
      override def writes(o: UserType): JsValue = o match {
        case Admin => JsString("Admin")
        case Common => JsString("Common")
        case _ => JsString("Common")
      }
    }
    implicit val userFormats: Format[User] = (
      (JsPath \ "username").format[String] and
        (JsPath \ "password").format[String] and
        (JsPath \ "userType").format[UserType] and
        (JsPath \ "id").format[Long])(User.apply, unlift(User.unapply))
  }
}

@Singleton
class AuthService @Inject()(protected val dbConfigProvider:DatabaseConfigProvider)
                           (implicit ec:ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] {

  import AuthBasic._
  import profile.api._

  private implicit val utType: BaseColumnType[UserType] = MappedColumnType.base[UserType,String](
    {
      case Admin => "Admin"
      case Common => "Common"
      case _ => "Common"
    },
    s => s.toUpperCase() match {
      case "ADMIN" => Admin
      case "COMMON" => Common
      case _ => Common
    }
  )

  private class Users(tag:Tag) extends Table[User](tag,"users") {
    def username = column[String]("username")
    def password = column[String]("password")
    def userType = column[UserType]("userType")
    def id = column[Long]("id",O.PrimaryKey,O.AutoInc)
    override def * = (username,password,userType,id) <> ((User.apply _).tupled, User.unapply)
  }

  private val users = TableQuery[Users]

  def check(username:String, password:String): Future[Option[User]] = {
    db.run(users.filter(i => i.username === username && i.password === password).result.headOption)
  }

  def schema: String = users.schema.createStatements.mkString(" ")

  def add(username:String,password:String,role:UserType): Future[Boolean] =
    for {
      userExist <- db.run(users.filter(_.username === username).exists.result)
      _ <- if (!userExist) db.run(users += User(username, password, role))
      else Future.successful(0)
    } yield !userExist

  def list: Future[Seq[User]] = db.run(users.result)

  def delete(id:Long): Future[Int] = db.run(users.filter(_.id === id).delete)

}

trait AuthController { self: AbstractController =>

  import AuthBasic._

  val auth: AuthService
  val cache: AsyncCacheApi
  implicit val ec: ExecutionContext

  @inline def message(c:String): Result = Ok(Json.obj("message" -> c))

  def authenticatedAction(userType: Seq[UserType])(request: Request[AnyContent]): Future[Either[User,Result]] = {
    authenticatedInQuery(request) orElse authenticatedInBase64(request) match {
      case None => Future(Right(Unauthorized(Json.obj("message"-> "NOT_AUTHORIZED"))
        .withHeaders(WWW_AUTHENTICATE -> "Basic")))
      case Some(eit) => eit match {
        case Left(v) => Future(Right(v))
        case Right((u,p)) => cache.getOrElseUpdate[Option[User]](s"user.$u") { auth.check(u,p) }.map {
          case Some(user) if userType.contains(user.userType) => Left(user)
          case None => Right(message("User token check failed."))
          case _ => Right(message("User not authorized, may not in super higher group."))
        }
      }
    }
  }

  def authenticatedInQuery(request:Request[AnyContent]): Option[Either[Result,(String,String)]] = {
    (for {
      user <- request.getQueryString("user")
      password <- request.getQueryString("password")
    } yield (user, password)) match {
      case None => None
      case Some(value) => Some(Right(value))
    }
  }

  def authenticatedInBase64(request:Request[AnyContent]):Option[Either[Result,(String,String)]] = {
    request.headers.get(HeaderNames.AUTHORIZATION).map { header =>
      val BasicHeader = "Basic (.*)".r
      header match {
        case BasicHeader(base64) =>
          try {
            new String(Base64.getDecoder.decode(base64)).split(":",2) match {
              case Array(u,p) => Right(u -> p)
              case _ => Left(BadRequest("Invalid basic authentication"))
            }
          } catch {
            case e: Throwable => Left(BadRequest(s"Invalid basic authentication ${e.getMessage}"))
          }
      }
    }
  }

  def authAdmin: Request[AnyContent] => Future[Either[User, Result]] = authenticatedAction(Seq(Admin))

  def authUsers: Request[AnyContent] => Future[Either[User, Result]] = authenticatedAction(Seq(Admin,Common))
}