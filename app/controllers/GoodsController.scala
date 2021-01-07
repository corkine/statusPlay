package controllers

import java.nio.file.Paths
import java.time.{Duration, LocalDateTime}
import java.time.format.DateTimeFormatter

import javax.inject.{Inject, Singleton}
import oss.OSSUtils
import play.api.Logger
import play.api.cache.AsyncCacheApi
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.Json
import play.api.mvc._
import services.{AuthController, AuthService, CurrentState, Good, GoodsRepository, Importance}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GoodsController @Inject()(cc: ControllerComponents, gr: GoodsRepository,
                                oss:OSSUtils,
                                val cache: AsyncCacheApi,
                                val auth: AuthService)
                               (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  val logI: String => Unit = Logger(getClass).info(_)

  /**
   * 查 - 搜索
   * @param like 查询 good ID 关键字
   * @param skip 跳过多少个条目-用于分页
   * @param limit 限制多少个条目-用于分页
   */
  def goods(like:Option[String],skip:Option[Int],limit:Option[Int]): Action[AnyContent] = Action.async { r =>
    ???
    Future(Ok("233").withSession("user" -> "Marvin","type" -> "Admin"))
  }

  /**
   * 查 - 单个
   * @param oGoodId Good Id
   */
  def good(oGoodId:String): Action[AnyContent] = Action.async { r =>
    val goodId = oGoodId.toUpperCase()
    authUsers(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) => gr.singleGood(goodId).map {
          case Left(e) => Ok(Json.obj("message" -> e, "status" -> 0, "good" -> None))
          case Right(g) => Ok(Json.obj("message" -> "done.", "status" -> 1, "good" -> g))
        }
    }
  }

  /**
   * 查 - 全部
   * @param lastDay 最近几天添加的数据
   * @param recentFirst 优先显示最近添加的
   * @param skip 跳过 - 用于分页
   * @param take 限制 - 用于分页
   */
  def goodsAll(lastDay:Option[Int],recentFirst:Option[Boolean],hideRemove:Option[Boolean],shortByName:Option[Boolean],skip:Option[Long],take:Option[Long]): Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(v) => Future(v)
      case Left(_) =>
        gr.allGoods(lastDay, recentFirst.getOrElse(true), hideRemove.getOrElse(false), shortByName.getOrElse(true), skip.getOrElse(0), take.getOrElse(1000)) map { res =>
          Ok(Json.toJson(res))
        }
    }
  }

  /**
   * 删
   * @param oGoodId Good Id
   * @return
   */
  def deleteGood(oGoodId:String): Action[AnyContent] = Action.async { r =>
    val goodId = oGoodId.toUpperCase()
    authAdmin(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) => gr.deleteGood(goodId).map {
        case Left(e) => Ok(Json.obj("message" -> e, "status" -> 0))
        case Right(in) => Ok(Json.obj("message" -> in, "status" -> 1))
      }
    }
  }

  /**
   * 增
   * 为 iOS 的捷径所设计，允许通过 POST multipartFormData or formUrlEncoded 来添加数据
   * 参数：goodId - 字符串，非必须，缺失时系统自动生成，不区分大小写，不允许为空
   * 参数：addTime - 字符串，ISO_DATE_TIME 格式，非必须，缺失时设定为当前值，不允许为空
   * 参数：name - 字符串，必须
   * 参数：description - 字符串，非必须，允许为空
   * 参数：kind - 字符串，非必须，允许为空
   * 参数：currentState - 字符串，不区分大小写，非必须，缺失时设定为 Ordinary，不允许为空
   * 参数：importance - 字符 ABCDEN，不区分大小写，非必须，缺失时设定为 N，不允许为空
   * 参数：validUntil - 字符串，ISO_DATE_TIME 格式，非必须，允许为空
   * 参数：estimatedLiftTime - 字符串，表示预计使用天数，非必须，允许为空
   * 参数：message - 字符串，表示公开显示的信息，非必须，允许为空
   * 参数：place - 字符串，表示此物品放置的位置，非必须，允许为空
   * 参数：picture - 图片文件，非必须，允许为空
   */
  def goodAdd: Action[AnyContent] = Action.async { r =>
    authUsers(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) =>
        val ans = r.body.asMultipartFormData.map { m => m.dataParts -> m.file("picture") }
          .orElse(r.body.asFormUrlEncoded.map { f => f -> None}) match {
          case Some((map,pic)) => genGoodAdd(map,pic)
          case None => Left("Can't parse multipartFormData or formUrlEncoded")
        }
        ans match {
          case Left(err) => Future(message(err))
          case Right(g) =>
            logI(s"Parsed goods $g now...")
            gr.addGoods(g).map {
              case Left(err) => message(err)
              case Right(ans) => Ok(Json.obj("message" -> "Add successful.", "status" -> 1, "good" -> ans))
            }
        }
    }
  }

  private def genGoodAdd(map:Map[String,Seq[String]],
                         pic:Option[MultipartFormData.FilePart[TemporaryFile]]): Either[String,Good] = {
    val goodId = map.get("goodId").map(_.head.toUpperCase).getOrElse(Good.randomUpperGoodID)
    val addTime = map.get("addTime").map(_.head)
        .flatMap(s => try {
          Some(LocalDateTime.parse(s,DateTimeFormatter.ISO_DATE_TIME))
        } catch {
          case _: Throwable => None
        }).getOrElse(LocalDateTime.now())
    val currentState = map.get("currentState")
        .flatMap(s => CurrentState.strUpper2CS(s.head))
        .getOrElse(CurrentState.Ordinary)
    val importance = map.get("importance")
        .flatMap(s => Importance.strUpper2IM(s.head))
        .getOrElse(Importance.N)
    val e_name = map.get("name").map(_.head).toRight("Can't parse name.")
    val m_desc = map.get("description").map(_.head)
    val m_validUntil = map.get("validUntil").map(_.head).flatMap(s => try {
          Some(LocalDateTime.parse(s,DateTimeFormatter.ISO_DATE))
        } catch { case _: Throwable => None })
    val m_estimatedLiftTime = map.get("estimatedLiftTime").flatMap(s => try {
          Some(Duration.ofDays(s.head.toLong))
        } catch { case _: Throwable => None })
    val m_kind = map.get("kind").map(_.head)
    val m_message = map.get("message").map(_.head)
    val m_place = map.get("place").map(_.head)
    val m_picture = pic.map { p =>
        if (p.fileSize == 0) Left("File size 0 Error.") else {
          val file = Paths.get(p.filename.replace("，","_")
            .replace(" ","_")).toFile
          p.ref.moveTo(file)
          val url = oss.upload(file,"goods")
          file.delete()
          if (url != null) Right(url) else Left("Upload Error.")
        }
    }
    (e_name,m_picture) match {
      case (Left(e1),Some(Left(e2))) => Left(e1 + "; " + e2)
      case (Left(e1),_) => Left(e1)
      case (Right(_),Some(Left(e2))) => Left(e2)
      case (Right(n),Some(Right(p))) => Right(Good(n,Some(p),m_desc,m_kind,currentState,importance,m_validUntil,
          m_estimatedLiftTime, m_message, m_place, addTime, id = goodId))
      case (Right(n),None) => Right(Good(n,None,m_desc,m_kind,currentState,importance,m_validUntil,
          m_estimatedLiftTime, m_message, m_place, addTime, id = goodId))
    }
  }

  /**
   * 改
   * 为 iOS 的捷径所设计，允许通过 POST multipartFormData or formUrlEncoded 来修改数据
   * 参数：addTime - 字符串，ISO_DATE_TIME 格式，非必须，缺失时设定为当前值，不允许为空
   * 参数：name - 字符串，必须
   * 参数：description - 字符串，非必须，允许为空
   * 参数：kind - 字符串，非必须，允许为空
   * 参数：currentState - 字符串，不区分大小写，非必须，缺失时设定为 Ordinary，不允许为空
   * 参数：importance - 字符 ABCDEN，不区分大小写，非必须，缺失时设定为 N，不允许为空
   * 参数：validUntil - 字符串，ISO_DATE 格式，非必须，允许为空
   * 参数：estimatedLiftTime - 字符串，表示预计使用天数，非必须，允许为空
   * 参数：message - 字符串，表示公开显示的信息，非必须，允许为空
   * 参数：place - 字符串，表示此物品放置的位置，非必须，允许为空
   * 参数：picture - 图片文件，非必须，允许为空
   * 参数：noPicDelete - 字符串，非必须，"1"表示没有上传 picture 图片时做删除更新，其他表示没有
   * 参数：newGoodId - 字符串，非必须，如果没有则不修改其 goodId 主键
   * 上传 picture 图片时做不更新之前的 picture 字段（之前可能有或者无图片）
   */
  def goodEdit(oGoodId:String): Action[AnyContent] = Action.async { r =>
    val goodId = oGoodId.toUpperCase
    authAdmin(r) flatMap {
      case Right(e) => Future(e)
      case Left(_) => gr.singleGood(goodId).flatMap {
          case Left(value) => Future(message(value))
          case Right(g) =>
            val ans = r.body.asMultipartFormData.map { m => m.dataParts -> m.file("picture") }
              .orElse(r.body.asFormUrlEncoded.map { f => f -> None}) match {
              case Some((map,pic)) => genGoodEdit(g,map,pic)
              case None => Left("Can't parse multipartFormData or formUrlEncoded")
            }
            ans match {
              case Left(err) => Future(message(err))
              case Right(g) =>
                logI(s"Parsed goods $g now...")
                gr.updateGood(goodId, g).map {
                  case Left(err) => message(err)
                  case Right(ans) => Ok(Json.obj("message" -> "Update successful.", "status" -> 1, "good" -> ans))
                }
            }
        }
    }
  }

  private def genGoodEdit(goodOld: Good,
                       map:Map[String,Seq[String]],
                       pic:Option[MultipartFormData.FilePart[TemporaryFile]]): Either[String,Good] = {
    val name = map.get("name").map(_.head).getOrElse(goodOld.name)
    val m_desc = map.get("description").map(_.head).orElse(goodOld.description)
    val m_kind = map.get("kind").map(_.head).orElse(goodOld.kind)
    val updatePic = map.get("noPicDelete").map(_.head).getOrElse("0") match {
      case "1" => true
      case _ => false
    }
    val currentState = map.get("currentState")
      .flatMap(s => CurrentState.strUpper2CS(s.head))
      .getOrElse(goodOld.currentState)
    val importance = map.get("importance")
      .flatMap(s => Importance.strUpper2IM(s.head))
      .getOrElse(goodOld.importance)
    val m_validUntil = map.get("validUntil").map(_.head).flatMap(s => try {
      Some(LocalDateTime.parse(s,DateTimeFormatter.ISO_DATE))
    } catch { case _: Throwable => None }).orElse(goodOld.validUntil)
    val m_estimatedLiftTime = map.get("estimatedLiftTime").flatMap(s => try {
      Some(Duration.ofDays(s.head.toLong))
    } catch { case _: Throwable => None }).orElse(goodOld.estimatedLiftTime)
    val updatedId = map.get("newGoodId").map(_.head.toUpperCase).getOrElse(goodOld.id)
    val addTime = map.get("addTime").map(_.head)
      .map(LocalDateTime.parse(_,DateTimeFormatter.ISO_DATE_TIME))
      .getOrElse(goodOld.addTime)
    val m_message = map.get("message").map(_.head).orElse(goodOld.message)
    val m_place = map.get("place").map(_.head).orElse(goodOld.place)
    val m_picture = pic.map { p =>
      if (p.fileSize == 0) Left("File size 0 Error.") else {
        val file = Paths.get(p.filename.replace("，","_")
          .replace(" ","_")).toFile
        p.ref.moveTo(file)
        val url = oss.upload(file,"goods")
        file.delete()
        if (url != null) Right(url) else Left("Upload Error.")
      }
    } match {
      case Some(Left(e)) => Left(e) //如果发生错误，则终止进程
      case Some(Right(p)) => Right(Some(p)) //如果有最新提交，则使用现在照片
      case None if !updatePic => Right(goodOld.picture) //如果没有最新提交，且不需要更新照片，则使用之前的照片（可能之前没有）
      case None if updatePic => Right(None) //如果没有最新提交，且需要更新照片，则删除照片
    }
    m_picture map { p => goodOld.copy(name = name, description = m_desc, kind = m_kind,
      currentState = currentState, importance = importance, validUntil = m_validUntil,
      estimatedLiftTime = m_estimatedLiftTime, message = m_message, place = m_place,
      addTime = addTime, updateTime = LocalDateTime.now(),
      picture = p, id = updatedId)
    }
  }

  /**
   * 显示内容详情：三种情况：任何权限下均可以检查是否 GOODID 是否存在系统中，登录权限下可查看产品，没有 GOODID 对应则进行提示。
   * @param oGoodId 不区分大小写的用户输入 ID
   */
  def goodDetail(oGoodId:String): Action[AnyContent] = Action.async { r =>
    val goodId = oGoodId.toUpperCase()
    authUsers(r) flatMap { r =>
      gr.singleGood(goodId).map {
        //case _ => Ok(views.html.details(None,auth = r.isLeft, id=goodId))
        case Left(_) => Ok(views.html.details(None,auth = r.isLeft, id=goodId))
        case Right(g) =>
          /*val fakeg = Good("好看的茶杯",Some("http://static2.mazhangjing.com/goods/20201016/8d3fbdc_Camera_2020-10-16_下午3.25.17.jpeg"),
            Some("双伟送的好看的茶杯"),Some("日用品"),CurrentState.NotActive,
            Importance.N,Some(LocalDateTime.now().plusDays(100)),Some(Duration.ofDays(100)))*/
          Ok(views.html.details(Some(g),auth = r.isLeft, id=goodId))
      }
    }
  }
  def schema: Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) => Future(Ok(gr.schema))
    }
  }
}