package services

import java.time.LocalDateTime

import com.google.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

case class Website(name:String, url:String, note:Option[String],
                   priority:Int, id:Long = 0L)
object Website {
  implicit val ww: Format[Website] = (
    (JsPath \ "name").format[String] and
      (JsPath \ "url").format[String] and
      (JsPath \ "note").formatNullable[String] and
      (JsPath \ "priority").format[Int] and
      (JsPath \ "id").format[Long])(Website.apply, unlift(Website.unapply))
}

trait Status
object Status {
  def fromString(status:String):Status = status match {
    case "OK" => Ok
    case "UNSTABLE" => UnStable
    case "OFFLINE" => Offline
    case _ => null
  }
  def toInt(status: Status):Int = status match {
    case Offline => 20
    case UnStable => 50
    case Ok => 100
  }
  def toStatus(st:Int):Status = st match {
    case _ if st < 30 => Offline
    case _ if st < 80 => UnStable
    case _ => Ok
  }
  implicit val sw: Format[Status] = new Format[Status] {
    override def reads(json: JsValue): JsResult[Status] = fromString(json.toString()) match {
      case null => JsError(s"Can't get from $json")
      case o => JsSuccess(o)
    }
    override def writes(o: Status): JsValue = JsString(o.toString)
  }
}
object Ok extends Status {
  override def toString: String = "OK"
}
object UnStable extends Status {
  override def toString: String = "UNSTABLE"
}
object Offline extends Status {
  override def toString: String = "OFFLINE"
}

case class Activity(website:Long, checkTime:LocalDateTime,
                    status:Status, note:Option[String], id:Long = 0L)
object Activity {
  implicit val aw: Format[Activity] =
  ((JsPath \ "website").format[Long] and
    (JsPath \ "checkTime").format[LocalDateTime] and
    (JsPath \ "status").format[Status] and
    (JsPath \ "note").formatNullable[String] and
    (JsPath \ "id").format[Long])(Activity.apply, unlift(Activity.unapply))
}

trait ActivityComponent { self: HasDatabaseConfigProvider[JdbcProfile] =>
  import profile.api._
  class WebsiteTable(tag:Tag) extends Table[Website](tag, "websites") {
    def name = column[String]("name")
    def url = column[String]("url")
    def note = column[Option[String]]("note")
    def priority = column[Int]("priority")
    def id = column[Long]("id",O.PrimaryKey, O.AutoInc)
    override def * = (name,url,note,priority,id) <> ((Website.apply _).tupled, Website.unapply)
  }

  lazy val websites = TableQuery[WebsiteTable]
  lazy val websitesInsert = websites returning websites.map(_.id)

  implicit val statusType = MappedColumnType.base[Status,String](_.toString, Status.fromString)

  class ActivityTable(tag:Tag) extends Table[Activity](tag,"activities") {
    def tup2Act(wid:Long, ct:LocalDateTime, st:Int, note:Option[String], id:Long): Activity =
      Activity(wid,ct,Status.toStatus(st),note,id)
    def act2Tuple(activity: Activity):Option[(Long,LocalDateTime,Int,Option[String],Long)] = {
      Some(activity.website,activity.checkTime,Status.toInt(activity.status), activity.note,activity.id)
    }

    def checkTime = column[LocalDateTime]("checkTime")
    def status = column[Int]("status")
    def note = column[Option[String]]("note")
    def id = column[Long]("id",O.PrimaryKey,O.AutoInc)
    def website_id = column[Long]("website_id")
    def website = foreignKey("website",website_id,websites)(_.id)
    override def * = (website_id, checkTime, status, note, id) <> ((tup2Act _).tupled, act2Tuple)
  }

  lazy val activities = TableQuery[ActivityTable]
  lazy val activitiesInsert = activities returning activities.map(_.id)
}

@Singleton
class ActivityRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
                                              (implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] with ActivityComponent {
  import profile.api._

  def allWebsites: Future[Seq[Website]] = db.run(websites.result)

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
  }

/*  /**
   * 获取所有网站包含的指定时间内的网站信息、平均状态、记录数目
   */
  def websitesInfo(from:LocalDateTime, to:LocalDateTime): Future[Seq[(Website, Option[Int], Int)]] = db.run {
    (websites joinLeft
      activities.filter(a => a.checkTime >= from && a.checkTime <= to)
      on (_.id === _.website_id))
      .groupBy { case (w,_) => w.id }
      .map { case (wId,g) => (wId, g.map(_._1.name).min -> g.map(_._2.map(_.status)).avg -> g.length)}
      .result.flatMap { seq =>
      DBIO.sequence(seq.map { case (id,info) =>
        websites.filter(_.id === id).result.head.map(w => (w, info._1, info._2))}
        :Seq[DBIOAction[(Website,Option[Int],Int),NoStream,Effect.All]])
    }
  }*/

  def schema: String = {
    val ws = websites.schema.createStatements
    val wsd = websites.schema.dropIfExistsStatements
    val ac = activities.schema.createStatements
    val acd = activities.schema.dropIfExistsStatements
    val sb = new StringBuilder
    sb.append(ws.mkString("\n"))
      .append("\n\n\n")
      .append(wsd.mkString("\n"))
      .append("\n\n\n")
      .append(ac.mkString("\n"))
      .append("\n\n\n")
      .append(acd.mkString("\n")).toString()
  }

  private def allWebsitesActivityWithInfoByZip(websiteId:Long, skip:Int = 0, limit:Int = 100):
  Future[Seq[(Website, Activity)]] = db.run {
    websites.filter(_.id === websiteId) zip activities.filter(_.website_id === websiteId) drop skip take limit result
  }

  private def allWebsitesActivityWithInfoByJoin(websiteSkip:Int = 0, websiteLimit:Int = 100,
                                                activityLastDays:Int = 2, activityLimit:Int = 50):
  Future[Map[Website, Option[Activity]]] = db.run {
    (websites.drop(websiteSkip).take(websiteLimit) joinLeft
      activities.filter(_.checkTime > LocalDateTime.now().minusDays(activityLastDays))
        .sortBy(_.checkTime.desc).take(activityLimit)
      on (_.id === _.website_id)).result
  }.map(_.toMap)
}
