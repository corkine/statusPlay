package services

import java.time.{Duration, LocalDateTime}

import com.google.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import services.Category.simpleField
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

/////////////////////////////////// NOT IOS HEALTH FORMAT BEAN ///////////////////////////////////
case class Record(day:LocalDateTime, calorie:Int, note:Option[String], id:Long)
object Record {
  implicit val rf: Format[Record] = (
    (JsPath \ "day").format[LocalDateTime] and
      (JsPath \ "calorie").format[Int] and
      (JsPath \ "note").formatNullable[String] and
      (JsPath \ "id").format[Long])(Record.apply, unlift(Record.unapply))
}

/////////////////////////////////// IOS HEALTH DATA FORMAT BEAN ///////////////////////////////////
trait Category
object Steps extends Category //步数
object VO2Max extends Category //最大耗氧量
object WalkingRunningDistance extends Category //步行+跑步距离
object FlightsClimbed extends Category //已爬楼层
object ActiveCalories extends Category //活动卡路里
object RestingCalories extends Category //静息卡路里
object HeartRate extends Category //心跳
object RestingHeartRate extends Category //静息心跳
object WalkingHeartRateAverage extends Category //步行平均心跳
object HeartRateVariability extends Category //心率变异性
object Weight extends Category
object Category {
  /**
   * 根据 Category Case Class 找到 iOS 捷径上传 JSON Key 名
   */
  def simpleField(in:Category): String =  in match {
    case Steps => "step"
    case VO2Max => "vo2"
    case WalkingRunningDistance => "distance"
    case FlightsClimbed => "floor"
    case ActiveCalories => "activeactivity"
    case RestingCalories => "restactivity"
    case HeartRate => "heart"
    case RestingHeartRate => "restheart"
    case WalkingHeartRateAverage => "walkheart"
    case HeartRateVariability => "heartvariability"
    case Weight => "weight"
  }

  /**
   * Category 和 String 的互相转换
   */
  def cats2String(in:Category): String = in.getClass.getSimpleName.replace("$","")

  /**
   * String 和 Category 的互相转换
   */
  def str2Cats(in:String):Category = in.toUpperCase match {
    case "STEPS" => Steps
    case "VO2MAX" => VO2Max
    case "WALKINGRUNNINGDISTANCE" => WalkingRunningDistance
    case "FLIGHTSCLIMBED" => FlightsClimbed
    case "ACTIVECALORIES" => ActiveCalories
    case "RESTINGCALORIES" => RestingCalories
    case "HEARTRATE" => HeartRate
    case "RESTINGHEARTRATE" => RestingHeartRate
    case "WALKINGHEARTRATEAVERAGE" => WalkingHeartRateAverage
    case "HEARTRATEVARIABILITY" => HeartRateVariability
    case "WEIGHT" => Weight
  }
}

case class Data(category:Category,value:Int,unit:String,
                start:LocalDateTime,end:LocalDateTime,duration:Duration,id:Long=0L)
object Data {
  /**
   * 根据 Controller 获得的 iOS 捷径上传的 JSON 解析为 Seq[Data]
   */
  def parseFieldJSON(in:JsValue,skipZeroValueData:Boolean = true):Seq[Data] = {
    val VALUE = "value"
    val UNIT = "unit"
    val START = "start"
    val END = "end"
    val DURATION = "duration"
    def dataFor(clazz:Category):Seq[Data] = {
      val vData = (in \ simpleField(clazz) \ VALUE).validate[String].get.split("\n")
      val unit = (in \ simpleField(clazz) \ UNIT).validate[String].get.split("\n").head
      val startData = (in \ simpleField(clazz) \ START).validate[String].get.split("\n")
      val endData = (in \ simpleField(clazz) \ END).validate[String].get.split("\n")
      val durationData = (in \ simpleField(clazz) \ DURATION).validate[String].get.split("\n")
      vData.indices.map { i =>
        val values = vData(i).toInt
        val starts = startData(i)
        val ends = endData(i)
        val durations = durationData(i)
        Data(clazz,values,unit,
          LocalDateTime.parse(starts),LocalDateTime.parse(ends),Duration.parse(durations))
      }
    }
    val data =
      dataFor(Steps) ++ dataFor(VO2Max) ++ dataFor(WalkingRunningDistance) ++
        dataFor(FlightsClimbed) ++ dataFor(ActiveCalories) ++ dataFor(RestingCalories) ++
        dataFor(HeartRate) ++ dataFor(RestingHeartRate) ++ dataFor(WalkingHeartRateAverage) ++
        dataFor(HeartRateVariability) //++ dataFor(Weight)
    if (skipZeroValueData) data.filter(_.value != 0) else data
  }
}

/////////////////////////////////// DAO  ///////////////////////////////////

trait FitnessComponent { self: HasDatabaseConfigProvider[JdbcProfile] =>
  import profile.api._
  class Records(tag: Tag) extends Table[Record](tag, "Records") {
    def day = column[LocalDateTime]("day")
    def calorie = column[Int]("calorie")
    def note = column[Option[String]]("note")
    def id = column[Long]("id",O.PrimaryKey,O.AutoInc)
    override def * = (day,calorie,note,id) <> ((Record.apply _).tupled, Record.unapply)
  }

  lazy val records = TableQuery[Records]
  lazy val recordsInsert = records returning records.map(_.id)

  implicit val catType =
    MappedColumnType.base[Category,String](Category.cats2String,Category.str2Cats)
  implicit val durationType =
    MappedColumnType.base[Duration,Long](_.getSeconds, Duration.ofSeconds)

  class Datas(tag:Tag) extends Table[Data](tag, "Datas") {
    def category = column[Category]("category")
    def value = column[Int]("value")
    def unit = column[String]("unit")
    def start = column[LocalDateTime]("startTime")
    def end = column[LocalDateTime]("endTime")
    def duration = column[Duration]("duration")
    def id = column[Long]("id",O.PrimaryKey,O.AutoInc)
    override def * =
      (category, value, unit, start, end, duration, id) <> ((Data.apply _).tupled, Data.unapply)
  }

  lazy val datas = TableQuery[Datas]
  lazy val datasInsert = datas returning datas.map(_.id)
}

/////////////////////////////////// SERVICE ///////////////////////////////////

@Singleton
class FitnessRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider,
                                   val ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] with FitnessComponent {
  import profile.api._

  ///////////////////// RECORDS //////////////////////

  def all(from:LocalDateTime,to:LocalDateTime): Future[Seq[Record]] = db.run {
    records.filter(o => o.day >= from && o.day <= to).result
  }

  def batchInsertData: DBIOAction[Seq[Int], NoStream, Effect.Write] = {
    val data:Seq[(LocalDateTime,Int,Option[String])] = null
    val ans = data.map { case (time,cal,note) =>
      records.map(r => (r.day,r.calorie,r.note))
        .forceInsertQuery(
          Query(time, cal, note).filterNot(_ =>
            records.filter(r => r.day =!= time && r.calorie =!= cal && r.note =!= note).exists))
    }
    DBIO.sequence(ans).withPinnedSession
  }

  def recordDetails(recordId:Long): Future[Option[Record]] = db.run {
    records.filter(_.id === recordId).result.headOption
  }

  ///////////////////// DATAS //////////////////////

  def insertBatch(js:JsValue): Future[Seq[Int]] = {
    val dta = Data.parseFieldJSON(js)
    db.run {
      DBIO.sequence(
        dta.map { data =>
          datas.map(d => (d.category,d.value,d.unit,d.start,d.end,d.duration))
            .forceInsertQuery(
              Query(data.category,data.value,data.unit,data.start,data.end,data.duration).filterNot(_ =>
                datas.filter(e => e.category === data.category && e.value === data.value &&
                  e.start === data.start && e.duration === data.duration
                ).exists
              )
            )
        }:Seq[DBIOAction[Int,NoStream,Effect.All]]
      )
    }
  }

  def schema: String = records.schema.createStatements.mkString("\n") + "\n\n" +
               records.schema.dropIfExistsStatements.mkString("\n") + "\n\n" +
               datas.schema.createStatements.mkString("\n") + "\n\n" +
               datas.schema.dropIfExistsStatements.mkString("\n")

}