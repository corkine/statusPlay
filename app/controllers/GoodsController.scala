package controllers

import java.io.File
import java.nio.file.{Files, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import javax.inject.{Inject, Singleton}
import oss.OSSUtils
import play.api.libs.json.Json
import play.api.mvc._
import services.{AuthController, AuthService, FitnessRepository, Good, GoodsRepository}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GoodsController @Inject()(cc: ControllerComponents, gr: GoodsRepository,
                                oss:OSSUtils, val auth: AuthService)
                               (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  /**
   * 查 - 搜索
   * @param like 查询 good ID 关键字
   * @param skip 跳过多少个条目-用于分页
   * @param limit 限制多少个条目-用于分页
   */
  def goods(like:Option[String],skip:Option[Long],limit:Option[Long]) = Action.async { r =>
    ???
  }

  /**
   * 查 - 单个
   * @param goodId Good Id
   */
  def good(goodId:Option[Long]) = Action.async { r =>
    ???
  }

  /**
   * 查 - 统计
   * @param lastDays 过去多少天内添加到系统的 Goods
   */
  def goodSum(lastDays:Option[Long]) = Action.async { r =>
    ???
  }

  /**
   * 删
   * @param goodId Good Id
   * @return
   */
  def deleteGood(goodId:Option[Long]) = Action.async { r =>
    ???
  }

  /**
   * 增
   */
    //,goodId:Option[String],picture:Option[Array[Byte]],description:Option[String],
    //             kind:Option[String],addTime:Option[LocalDateTime]
  def addGood = Action.async { r =>
    val ans = r.body.asMultipartFormData.map { b =>
      val d = b.dataParts
      val name = d.get("name").map(_.head)
      val goodId = d.get("goodId").map(_.head)
      val desc = d.get("description").map(_.head)
      val kind = d.get("kind").map(_.head)
      val addTime = d.get("addTime").map(_.head).map(LocalDateTime.parse(_,DateTimeFormatter.ISO_DATE_TIME))
      println(name,goodId,desc,kind,addTime)
      //除了 name 不能为空，其余均可以为空
      val mayPicture = b.file("picture").map { p =>
        if (p.fileSize == 0) Left("File size 0 Error.") else {
          val file = Paths.get(p.filename).toFile
          p.ref.moveTo(file)
          val url = oss.upload(file)
          file.delete()
          if (url != null) Right(url) else Left("Upload Error.")
        }
      }
      name match {
        case Some(n) => mayPicture match {
          case Some(Left(err)) => Left(s"Can't parse picture. $err")
          case Some(Right(url)) => goodId match {
            case Some(sid) =>
              Right(Good(n,Some(url),desc,kind,addTime.getOrElse(LocalDateTime.now()),id=sid))
            case None =>
              Right(Good(n,Some(url),desc,kind,addTime.getOrElse(LocalDateTime.now())))
          }
          case None => goodId match {
            case Some(sid) =>
              Right(Good(n,None,desc,kind,addTime.getOrElse(LocalDateTime.now()),id=sid))
            case None =>
              Right(Good(n,None,desc,kind,addTime.getOrElse(LocalDateTime.now())))
          }
        }
        case None => Left("Can't parse name.")
      }
    }.getOrElse(Left("Can't parse to multipartData"))
    ans match {
      case Left(err) => Future(Ok(err))
      case Right(g) => Future(Ok(Json.toJson(g)))
    }
  }

  /**
   * 增
   * @param goodsId 需要修改的 Goods ID
   * @param picture 图片
   * @param description 描述
   * @param name 名称
   * @param kind 类别
   * @param addTime 添加时间
   */
  def editGood(goodsId:String,name:Option[String],picture:Option[Array[Byte]],description:Option[String],
              kind:Option[String],addTime:Option[LocalDateTime]) = Action.async { r =>
    ???
  }

  def test = Action { r =>
    Ok(oss.bucketName)
  }
}