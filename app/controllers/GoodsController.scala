package controllers

import java.nio.file.Paths
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import javax.inject.{Inject, Singleton}
import oss.OSSUtils
import play.api.Logger
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.Json
import play.api.mvc._
import services.{AuthController, AuthService, Good, GoodsRepository}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GoodsController @Inject()(cc: ControllerComponents, gr: GoodsRepository,
                                oss:OSSUtils, val auth: AuthService)
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
  }

  /**
   * 查 - 单个
   * @param goodId Good Id
   */
  def good(goodId:String): Action[AnyContent] = Action.async { r =>
    authUsers(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) => gr.singleGood(goodId).map {
          case Left(e) => Ok(Json.obj("message" -> e, "status" -> 0, "good" -> None))
          case Right(g) => Ok(Json.obj("message" -> "done.", "status" -> 1, "good" -> g))
        }
    }
  }

  /**
   * 查 - 统计
   */
  def goodsAll(lastDay:Option[Int],recentFirst:Option[Boolean],skip:Option[Long],take:Option[Long]): Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(v) => Future(v)
      case Left(_) =>
        gr.allGoods(lastDay, recentFirst.getOrElse(true), skip.getOrElse(0), take.getOrElse(1000)) map { res =>
          Ok(Json.toJson(res))
        }
    }
  }

  /**
   * 删
   * @param goodId Good Id
   * @return
   */
  def deleteGood(goodId:String): Action[AnyContent] = Action.async { r =>
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
    val goodId = map.get("goodId").map(_.head).getOrElse(Good.randomGoodID)
    val addTime = map.get("addTime").map(_.head)
      .map(LocalDateTime.parse(_,DateTimeFormatter.ISO_DATE_TIME))
      .getOrElse(LocalDateTime.now())
    val e_name = map.get("name").map(_.head).toRight("Can't parse name.")
    val m_desc = map.get("description").map(_.head)
    val m_kind = map.get("kind").map(_.head)
    val m_picture = pic.map { p =>
      if (p.fileSize == 0) Left("File size 0 Error.") else {
        val file = Paths.get(p.filename.replace("，","_")
          .replace(" ","_")).toFile
        p.ref.moveTo(file)
        val url = oss.upload(file)
        file.delete()
        if (url != null) Right(url) else Left("Upload Error.")
      }
    }
    (e_name,m_picture) match {
      case (Left(e1),Some(Left(e2))) => Left(e1 + "; " + e2)
      case (Left(e1),_) => Left(e1)
      case (Right(_),Some(Left(e2))) => Left(e2)
      case (Right(n),Some(Right(p))) => Right(Good(n,Some(p),m_desc,m_kind,addTime,id=goodId))
      case (Right(n),None) => Right(Good(n,None,m_desc,m_kind,addTime,id=goodId))
    }
  }

  /**
   * 改
   * @param goodsId 需要修改的 Goods ID
   */
  def editGood(goodsId:String): Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(e) => Future(e)
      case Left(_) => gr.singleGood(goodsId).flatMap {
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
                gr.updateGood(g).map {
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
    val addTime = map.get("addTime").map(_.head)
      .map(LocalDateTime.parse(_,DateTimeFormatter.ISO_DATE_TIME))
      .getOrElse(goodOld.addTime)
    val m_picture = pic.map { p =>
      if (p.fileSize == 0) Left("File size 0 Error.") else {
        val file = Paths.get(p.filename.replace("，","_")
          .replace(" ","_")).toFile
        p.ref.moveTo(file)
        val url = oss.upload(file)
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
      addTime = addTime, updateTime = LocalDateTime.now(), picture = p)
    }
  }

  def schema: Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) => Future(Ok(gr.schema))
    }
  }
}