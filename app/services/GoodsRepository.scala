package services

import java.time.LocalDateTime
import java.util.UUID

import com.google.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

case class Good(name:String,
                picture:Option[String], description:Option[String], kind:Option[String],
                addTime:LocalDateTime = LocalDateTime.now(),
                updateTime:LocalDateTime = LocalDateTime.now(),
                id:String = Good.randomGoodID)

case class GoodLog(name:String, description:Option[String], createAt:LocalDateTime, goodId: String, logId:Long = 0L)

object Good {
  def randomGoodID: String = UUID.randomUUID().toString.substring(0,7).toUpperCase
  implicit val goodF: Format[Good] =
    ((JsPath \ "name").format[String] and
      (JsPath \ "picture").formatNullable[String] and
      (JsPath \ "description").formatNullable[String] and
      (JsPath \ "kind").formatNullable[String] and
      (JsPath \ "addTime").format[LocalDateTime] and
      (JsPath \ "updateTime").format[LocalDateTime] and
      (JsPath \ "id").format[String])(Good.apply, unlift(Good.unapply))
}

object GoodLog {
  implicit val goodLogF: Format[GoodLog] =
    ((JsPath \ "name").format[String] and
      (JsPath \ "description").formatNullable[String] and
      (JsPath \ "createAt").format[LocalDateTime] and
      (JsPath \ "goodId").format[String] and
      (JsPath \ "logId").format[Long])(GoodLog.apply,unlift(GoodLog.unapply))
}

trait GoodsComponent { self: HasDatabaseConfigProvider[JdbcProfile] =>
  import profile.api._
  class GoodTable(tag:Tag) extends Table[Good](tag, "goods") {
    def name = column[String]("name")
    def picture = column[Option[String]]("picture")
    def description = column[Option[String]]("description")
    def kind = column[Option[String]]("kind")
    def addTime = column[LocalDateTime]("addTime")
    def updateTime = column[LocalDateTime]("updateTime")
    def id = column[String]("id",O.PrimaryKey)
    override def * =
      (name,picture,description,kind,addTime,updateTime,id) <> ((Good.apply _).tupled, Good.unapply)
  }

  lazy val goods = TableQuery[GoodTable]
  lazy val goodsInsert = goods returning goods.map(_.id)

  class GoodLogTable(tag:Tag) extends Table[GoodLog](tag, "goodLogs") {
    def name = column[String]("name")
    def description = column[Option[String]]("description")
    def createAt = column[LocalDateTime]("createAt")
    def logId = column[Long]("logId",O.PrimaryKey, O.AutoInc)
    def goodId = column[String]("goodId")
    def good = foreignKey("good_log_fk",goodId,goods)(_.id,
        onUpdate = ForeignKeyAction.Restrict,
        onDelete = ForeignKeyAction.Cascade)
    override def * =
      (name,description,createAt,goodId,logId) <> ((GoodLog.apply _).tupled, GoodLog.unapply)
  }

  lazy val goodLogs = TableQuery[GoodLogTable]
  lazy val goodLogsInsert = goodLogs returning goodLogs.map(_.logId)
}

@Singleton
class GoodsRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
                                   (implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] with GoodsComponent {
  import profile.api._

  def allGoods(lastDay:Option[Int],recentFirst:Boolean,skip:Long,take:Long): Future[Seq[Good]] =
    db.run(lastDay match {
      case None => goods
        .sortBy(i => if (recentFirst) i.addTime.desc else i.addTime.asc)
        .drop(skip).take(take)
        .result
      case Some(day) => goods
        .filter(_.addTime >= LocalDateTime.now().minusDays(day))
        .sortBy(i => if (recentFirst) i.addTime.desc else i.addTime.asc)
        .drop(skip).take(take)
        .result
    })

  def singleGood(goodId:String): Future[Either[String,Good]] = db.run {
    goods.filter(_.id === goodId).result.headOption
  }.map(_.toRight(s"Can't find this goodId $goodId."))

  def deleteGood(goodId:String): Future[Either[String,String]] = db.run {
    goods.filter(_.id === goodId).delete
  }.map {
    case 0 => Left(s"Can't find this id $goodId")
    case o => Right(s"Delete $o rows done.")
  }

  def addGoods(good:Good): Future[Either[String,Good]] =
    db.run(goods += good)
      .map(_ => Right(good))
      .recover {
        case e:Throwable => Left(s"Add Failure. ${e.getMessage}")
      }

  def updateGood(good:Good) =
    db.run(goods.filter(_.id === good.id).update(good))
      .map {
        case 0 => Left(s"Can't update this good ${good.id}")
        case _ => Right(good)
      }
  /*
    def addWebsite(name:String,url:String,note:Option[String],priority:Int): Future[Int] = db.run {
      websites.map(ex => (ex.name, ex.url, ex.note, ex.priority))
        .forceInsertQuery(Query((name,url,note,priority)).filterNot(_ =>
          websites.filter(_.url === url).exists
        ))
    }

    def deleteWebsite(id:Long): Future[Int] = db.run {
      websites.filter(_.id === id).delete
    }

    def updateWebsite(id:Long,name:Option[String],url:Option[String],note:Option[String],priority:Option[Int]): Future[Int] = db.run {
      websites.filter(_.id === id).result.headOption flatMap({
        case None => DBIO.successful(0)
        case Some(oldWebsite) =>
          val oName = name.getOrElse(oldWebsite.name)
          val oUrl = url.getOrElse(oldWebsite.url)
          val oNote = if (note.isEmpty) oldWebsite.note else note
          val oPriority = priority.getOrElse(oldWebsite.priority)
          websites.filter(_.id === id)
            .map(o => (o.name,o.url,o.note,o.priority))
            .update((oName,oUrl,oNote,oPriority))
      }:Option[Website] => DBIOAction[Int,NoStream,Effect.All])
    }

    def logBatchActivity(data: Seq[(Website, Status, Option[String])]): Future[Seq[Long]] = db.run {
      activitiesInsert ++= data.map(d => Activity(d._1.id,LocalDateTime.now(),d._2,d._3))
    }

    def logNewActivity(website:Long, checkTime:LocalDateTime, status:Status, note:Option[String]): Future[Int] = db.run {
      activities
        .map(a => (a.website_id, a.checkTime, a.status, a.note))
        .forceInsertQuery(
          Query(website, checkTime, Status.toInt(status), note).filter(_ =>
            websites.map(_.id).filter(_ === website).exists)
        )
    }

    /**
     * 获取指定网站分页查询的所有 Activity 数据（直接检索 Activity 表）
     */
    def websiteActivity(websiteId:Long, skip:Int = 0, limit:Int = 100): Future[Seq[Activity]] = db.run {
      activities.filter(_.website_id === websiteId).drop(skip).take(limit).result
    }

    /**
     * 获取指定网站分页查询的所有 Activity 数据（先查询是否存在网站 id）
     */
    def websiteActivitiesWithInfoByAction(websiteId:Long, skip:Option[Int], limit:Option[Int]):
    Future[Either[String,(Website,Seq[Activity])]] = db.run {
      websites.filter(_.id === websiteId).result.headOption flatMap({
        case None => DBIO.successful(Left(s"Can't find website Id $websiteId in database."))
        case Some(w) => activities.filter(_.website_id === websiteId)
          .sortBy(_.checkTime.desc).drop(skip.getOrElse(0)).take(limit.getOrElse(100)).result map { as => Right(w -> as) }
      }: Option[Website] => DBIOAction[Either[String,(Website,Seq[Activity])],NoStream,Effect.All])
    }

    /**
     * 获取所有网站在指定时间段内的所有 Activity 条目
     */
    def allWebsitesActivityWithInfoBetween(from:LocalDateTime, to:LocalDateTime, activityLimit:Int):
    Future[Map[Website, Activity]] = db.run {
      (for {
        w <- websites
        a <- activities.sortBy(_.checkTime).take(activityLimit)
        if a.website_id === w.id && a.checkTime >= from && a.checkTime <= to
      } yield (w,a)).result
    } map(_.toMap)


    /**
     * 获取所有网站包含的指定时间内的网站信息、平均状态、记录数目
     */
    def websitesInfo(from:LocalDateTime, to:LocalDateTime):
    Future[Seq[(Option[String], Option[String], Option[String], Long, Option[Int], Int)]] = db.run {
      (websites joinLeft
        activities.filter(a => a.checkTime >= from && a.checkTime <= to)
        on (_.id === _.website_id))
        .groupBy { case (w,_) => w.id }
        .map { case (wId,g) => (g.map(_._1.name).min,
          g.map(_._1.url).min,
          g.map(_._1.note).min,
          wId,
          g.map(_._2.map(_.status)).avg,
          g.length)}
        .result
    }*/

  def schema: String = {
    val ws = goods.schema.createStatements
    val wsd = goods.schema.dropIfExistsStatements
    val ac = goodLogs.schema.createStatements
    val acd = goodLogs.schema.dropIfExistsStatements
    val sb = new StringBuilder
    sb.append(ws.mkString("\n"))
      .append("\n\n\n")
      .append(wsd.mkString("\n"))
      .append("\n\n\n")
      .append(ac.mkString("\n"))
      .append("\n\n\n")
      .append(acd.mkString("\n")).toString()
  }
}
