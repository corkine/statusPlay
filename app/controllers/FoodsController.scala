package controllers

import java.nio.file.Paths
import java.time.{DayOfWeek, Duration, LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.Temporal

import javax.inject.{Inject, Singleton}
import oss.OSSUtils
import play.api.Logger
import play.api.cache.AsyncCacheApi
import play.api.libs.Files.TemporaryFile
import play.api.libs.json.Json
import play.api.mvc._
import services.{AuthController, AuthService, CurrentState, Food, FoodsRepository, Good, GoodsRepository, Importance}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FoodsController @Inject()(cc: ControllerComponents, fr: FoodsRepository,
                                oss:OSSUtils,
                                val cache: AsyncCacheApi,
                                val auth: AuthService)
                               (implicit val ec: ExecutionContext)
  extends AbstractController(cc) with AuthController {

  val logI: String => Unit = Logger(getClass).info(_)

  /**
   * 查 - 搜索
   * @param like 查询 food Name Or Description 关键字
   */
  def foods(like:String, addInRecentDay:Option[Int]): Action[AnyContent] = Action.async { r =>
    authUsers(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) => fr.findFood(like,addInRecentDay).map { res =>
        Ok(Json.obj("message" -> "done.", "status" -> (if (res.nonEmpty) 1 else 0), "foods" -> res))
      }
    }
  }

  /**
   * 查 - 单个
   * @param foodId Food Id
   */
  def food(foodId:Long): Action[AnyContent] = Action.async { r =>
    authUsers(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) => fr.singleFood(foodId).map {
        case Left(e) => Ok(Json.obj("message" -> e, "status" -> 0, "food" -> None))
        case Right(f) => Ok(Json.obj("message" -> "done.", "status" -> 1, "food" -> f))
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
  def foodsAll(lastDay:Option[Int],recentFirst:Option[Boolean],skip:Option[Long],take:Option[Long]): Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(v) => Future(v)
      case Left(_) =>
        fr.allFoods(lastDay, recentFirst.getOrElse(true), skip.getOrElse(0), take.getOrElse(1000)) map { res =>
          Ok(Json.toJson(res))
        }
    }
  }

  /**
   * 删
   * @param foodId Good Id
   * @return
   */
  def deleteFood(foodId:Long): Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) =>  fr.deleteFood(foodId).map {
        case Left(e) => Ok(Json.obj("message" -> e, "status" -> 0))
        case Right(in) => Ok(Json.obj("message" -> in, "status" -> 1))
      }
    }
  }

  /**
   * 增
   * 为 iOS 的捷径所设计，允许通过 POST multipartFormData or formUrlEncoded 来添加数据
   * 参数：addTime - 字符串，ISO_DATE_TIME 格式，非必须，缺失时设定为当前值，不允许为空
   * 参数：finishTime - 吃完时间，ISO_DATE_TIME 格式，非必须，默认为空
   * 参数：name - 字符串，必须
   * 参数：description - 字符串，非必须，允许为空
   * 参数：kind - 字符串，非必须，允许为空
   * 参数：buyEatIntervalDay - 购买到吃的间隔天数 - 0-N，非必须，默认为 0 天
   * 参数：evilDegree - 邪恶度，1-5，非必须，默认为空
   * 参数：hungerDegree - 饥饿度，1-5，非必须，默认为空
   * 参数：picture - 图片文件，非必须，允许为空
   */
  def foodAdd: Action[AnyContent] = Action.async { r =>
    authUsers(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) =>
        val ans = r.body.asMultipartFormData.map { m => m.dataParts -> m.file("picture") }
          .orElse(r.body.asFormUrlEncoded.map { f => f -> None}) match {
          case Some((map,pic)) => genFoodAdd(map,pic)
          case None => Left("Can't parse multipartFormData or formUrlEncoded")
        }
        ans match {
          case Left(err) => Future(message(err))
          case Right(f) =>
            logI(s"Parsed foods $f now...")
            fr.addFoods(f).map {
              case Left(err) => message(err)
              case Right(ans) => Ok(Json.obj("message" -> "Add successful.", "status" -> 1, "food" -> ans))
            }
        }
    }
  }

  private def genFoodAdd(map:Map[String,Seq[String]],
                         pic:Option[MultipartFormData.FilePart[TemporaryFile]]): Either[String,Food] = {
    val addTime = map.get("addTime").map(_.head)
      .flatMap(s => try {
        Some(LocalDateTime.parse(s,DateTimeFormatter.ISO_DATE_TIME))
      } catch {
        case _: Throwable => None
      }).getOrElse(LocalDateTime.now())
    val m_finishTime = map.get("finishTime").map(_.head)
      .flatMap(s => try {
        Some(LocalDateTime.parse(s,DateTimeFormatter.ISO_DATE_TIME))
      } catch {
        case _: Throwable => None
      })
    val e_name = map.get("name").map(_.head).toRight("Can't parse name.")
    val m_desc = map.get("description").map(_.head)
    val m_kind = map.get("kind").map(_.head)
    val buyEatIntervalDay = map.get("buyEatIntervalDay").flatMap(b => try {
      Some(b.head.toInt) } catch { case _: Throwable => None}).getOrElse(0)
    val m_evilDegree = map.get("evilDegree").flatMap(b => try {
      val ans = b.head.toInt
      if (ans >= 1 && ans <= 5) Some(ans) else None
    } catch { case _: Throwable => None})
    val m_hungerDegree = map.get("hungerDegree").flatMap(b => try {
      val ans = b.head.toInt
      if (ans >= 1 && ans <= 5) Some(ans) else None
    } catch { case _: Throwable => None})
    val m_picture = pic.map { p =>
      if (p.fileSize == 0) Left("File size 0 Error.") else {
        val file = Paths.get(p.filename.replace("，","_")
          .replace(" ","_")).toFile
        p.ref.moveTo(file)
        val url = oss.upload(file,"foods")
        file.delete()
        if (url != null) Right(url) else Left("Upload Error.")
      }
    }
    (e_name,m_picture) match {
      case (Left(e1),Some(Left(e2))) => Left(e1 + "; " + e2)
      case (Left(e1),_) => Left(e1)
      case (Right(_),Some(Left(e2))) => Left(e2)
      case (Right(n),Some(Right(p))) => Right(
        Food(n,Some(p),m_desc,m_kind,buyEatIntervalDay,m_evilDegree,m_hungerDegree,addTime,m_finishTime))
      case (Right(n),None) => Right(
        Food(n,None,m_desc,m_kind,buyEatIntervalDay,m_evilDegree,m_hungerDegree,addTime,m_finishTime))
    }
  }

  /**
   * 改
   * 为 iOS 的捷径所设计，允许通过 POST multipartFormData or formUrlEncoded 来修改数据
   * 参数：addTime - 字符串，ISO_DATE_TIME 格式，非必须，缺失时设定为当前值，不允许为空
   * 参数：finishTime - 字符串，ISO_DATE_TIME 格式，非必须，可为空
   * 参数：name - 字符串，必须
   * 参数：description - 字符串，非必须，允许为空
   * 参数：kind - 字符串，非必须，允许为空
   * 参数：buyEatIntervalDay - 购买到吃的间隔天数 - 0-N，非必须，默认为 0 天
   * 参数：evilDegree - 邪恶度，1-5，非必须，默认为空
   * 参数：hungerDegree - 饥饿度，1-5，非必须，默认为空
   * 参数：noPicDelete - 字符串，非必须，"1"表示没有上传 picture 图片时做删除更新，其他表示没有
   * 上传 picture 图片时做不更新之前的 picture 字段（之前可能有或者无图片）
   */
  def foodEdit(foodId:Long): Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(e) => Future(e)
      case Left(_) => fr.singleFood(foodId).flatMap {
        case Left(value) => Future(message(value))
        case Right(f) =>
          val ans = r.body.asMultipartFormData.map { m => m.dataParts -> m.file("picture") }
            .orElse(r.body.asFormUrlEncoded.map { f => f -> None}) match {
            case Some((map,pic)) => genFoodEdit(f,map,pic)
            case None => Left("Can't parse multipartFormData or formUrlEncoded")
          }
          ans match {
            case Left(err) => Future(message(err))
            case Right(f) =>
              logI(s"Parsed foods $f now...")
              fr.updateFood(foodId,f).map {
                case Left(err) => message(err)
                case Right(ans) => Ok(Json.obj("message" -> "Update successful.", "status" -> 1, "food" -> ans))
              }
          }
      }
    }
  }

  private def genFoodEdit(foodOld: Food,
                          map:Map[String,Seq[String]],
                          pic:Option[MultipartFormData.FilePart[TemporaryFile]]): Either[String,Food] = {
    val name = map.get("name").map(_.head).getOrElse(foodOld.name)
    val m_desc = map.get("description").map(_.head).orElse(foodOld.description)
    val m_kind = map.get("kind").map(_.head).orElse(foodOld.kind)
    val buyEatIntervalDay = map.get("buyEatIntervalDay").flatMap(b => try {
      Some(b.head.toInt) } catch { case _: Throwable => None}).getOrElse(foodOld.buyEatIntervalDay)
    val m_evilDegree = map.get("evilDegree").flatMap(b => try {
      val ans = b.head.toInt
      if (ans >= 1 && ans <= 5) Some(ans) else None
    } catch { case _: Throwable => None}).orElse(foodOld.evilDegree)
    val m_hungerDegree = map.get("hungerDegree").flatMap(b => try {
      val ans = b.head.toInt
      if (ans >= 1 && ans <= 5) Some(ans) else None
    } catch { case _: Throwable => None}).orElse(foodOld.evilDegree)
    val updatePic = map.get("noPicDelete").map(_.head).getOrElse("0") match {
      case "1" => true
      case _ => false
    }
    val addTime = map.get("addTime").map(_.head)
      .flatMap(s => try {
        Some(LocalDateTime.parse(s,DateTimeFormatter.ISO_DATE_TIME))
      } catch {
        case _: Throwable => None
      }).getOrElse(foodOld.addTime)
    val m_finishTime = map.get("finishTime").map(_.head)
      .flatMap(s => try {
        Some(LocalDateTime.parse(s,DateTimeFormatter.ISO_DATE_TIME))
      } catch {
        case _: Throwable => None
      }).orElse(foodOld.finishTime)
    val m_picture = pic.map { p =>
      if (p.fileSize == 0) Left("File size 0 Error.") else {
        val file = Paths.get(p.filename.replace("，","_")
          .replace(" ","_")).toFile
        p.ref.moveTo(file)
        val url = oss.upload(file,"foods")
        file.delete()
        if (url != null) Right(url) else Left("Upload Error.")
      }
    } match {
      case Some(Left(e)) => Left(e) //如果发生错误，则终止进程
      case Some(Right(p)) => Right(Some(p)) //如果有最新提交，则使用现在照片
      case None if !updatePic => Right(foodOld.picture) //如果没有最新提交，且不需要更新照片，则使用之前的照片（可能之前没有）
      case None if updatePic => Right(None) //如果没有最新提交，且需要更新照片，则删除照片
    }
    m_picture map { p => foodOld.copy(name = name, picture = p, description = m_desc, kind = m_kind,
      buyEatIntervalDay = buyEatIntervalDay,evilDegree = m_evilDegree, hungerDegree = m_hungerDegree,
      addTime = addTime, finishTime = m_finishTime)
    }
  }

  /**
   * 显示内容详情：三种情况：任何权限下均可以检查是否 FOODID 是否存在系统中，登录权限下可查看产品，没有 FOODID 对应则进行提示。
   * @param foodId 不区分大小写的用户输入 ID
   */
  def foodDetail(foodId:Long): Action[AnyContent] = Action.async { r =>
    authUsers(r) flatMap { r => fr.singleFood(foodId).map {
      case Left(_) => Ok(views.html.foodDetails(None,auth = r.isLeft, id=foodId))
      case Right(g) =>
        /*val fakeg = Good("好看的茶杯",Some("http://static2.mazhangjing.com/goods/20201016/8d3fbdc_Camera_2020-10-16_下午3.25.17.jpeg"),
          Some("双伟送的好看的茶杯"),Some("日用品"),CurrentState.NotActive,
          Importance.N,Some(LocalDateTime.now().plusDays(100)),Some(Duration.ofDays(100)))*/
        Ok(views.html.foodDetails(Some(g),auth = r.isLeft, id=foodId))
      }
    }
  }

  /**
   * 显示内容详情：查询的 FOOD LIST
   */
  def foodsDetail(today:Option[Boolean],week:Option[Boolean],month:Option[Boolean],day:Option[Int],kind:Option[String]): Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Left(_) => if ((today orElse week orElse month orElse day).isEmpty) {
        Future(message("You need have param: today, week, month or day"))
      } else {
        val beforeDay = today match {
          case Some(true) => LocalDate.now().atStartOfDay()
          case _ => week match {
            case Some(true) => LocalDate.now().`with`(DayOfWeek.MONDAY).atStartOfDay()
            case _ => month match {
              case Some(true) => LocalDate.now().withDayOfMonth(1).atStartOfDay()
              case _ => day match {
                case Some(d) => LocalDateTime.now().minusDays(d)
                case _ => throw new RuntimeException("Can't happen.")
              }
            }
          }
        }
        fr.rangeFoods(kind,beforeDay) map { res =>
          Ok(views.html.foodList(res, auth = true))
        }
      }
      case Right(value) => Future(value)
    }
  }

  def schema: Action[AnyContent] = Action.async { r =>
    authAdmin(r) flatMap {
      case Right(value) => Future(value)
      case Left(_) => Future(Ok(fr.schema))
    }
  }
}