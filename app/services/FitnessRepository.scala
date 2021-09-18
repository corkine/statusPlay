package services

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDateTime}
import java.util.Locale

import com.google.inject.{Inject, Singleton}
import org.slf4j.LoggerFactory
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

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
object Weight extends Category //体重
object Breath extends Category //正念呼吸
object Category {
  implicit val categoryFormatter: Format[Category] = new Format[Category] {
    override def reads(json: JsValue): JsResult[Category] = str2Cats(json.as[String]) match {
      case null => JsError(s"Can't parse $json")
      case o => JsSuccess(o)
    }
    override def writes(o: Category): JsValue = JsString(cats2String(o))
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
    case "BREATH" => Breath
    case _ => null
  }
}

case class Data(category:Category,value:Double,unit:String,
                start:LocalDateTime,end:LocalDateTime,duration:Duration,id:Long=0L)
object Data {
  implicit val dataFormatter: Format[Data] =
    ((JsPath \ "category").format[Category] and
      (JsPath \ "value").format[Double] and
      (JsPath \ "unit").format[String] and
      (JsPath \ "start").format[LocalDateTime] and
      (JsPath \ "end").format[LocalDateTime] and
      (JsPath \ "duration").format[Duration] and
      (JsPath \ "id").format[Long])(Data.apply, unlift(Data.unapply))
  private val logger = LoggerFactory.getLogger(getClass)
  /**
   * 根据 Controller 获得的 iOS 捷径上传的 JSON 解析为 Seq[Data]
   */
  def parseFieldJSON(in:JsValue,skipZeroValueData:Boolean = true):Seq[Data] = {
    Locale.setDefault(Locale.SIMPLIFIED_CHINESE)
    @inline def simpleField(in:Category): String =  in match {
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
      case Breath => "breath"
    }
    val VALUE = "value"
    val UNIT = "unit"
    val START = "start"
    val END = "end"
    val DURATION = "duration"
    val formatter = DateTimeFormatter.ofPattern("yyyy年M月d日 ah:mm")
    def parseDuration(in:String): Duration = {
      if (in.trim == "0" || in.isEmpty) return Duration.ZERO
      val s = in.split(":")
      if (s.length == 3) //1:13:22
        Duration.ofSeconds(s(0).toLong * 60 * 60 + s(1).toLong * 60 + s(2).toLong)
      else if (s.length == 2)  //23:11
        Duration.ofSeconds(s(0).toLong * 60 + s(1).toLong)
      else //23
        Duration.ofSeconds(s(0).toLong)
    }
    def dataFor(clazz:Category):Seq[Data] = {
      logger.info(s"Get data from ${Category.cats2String(clazz)}")
      try {
        val vData = (in \ simpleField(clazz) \ VALUE).validate[String].get.split("\n")
        val unit = (in \ simpleField(clazz) \ UNIT).validate[String].get.split("\n").head
        val startData = (in \ simpleField(clazz) \ START).validate[String].get.split("\n")
        val endData = (in \ simpleField(clazz) \ END).validate[String].get.split("\n")
        val durationData = (in \ simpleField(clazz) \ DURATION).validate[String].get.split("\n")
        vData.indices.map { i =>
          val values = vData(i).toDouble
          val starts = startData(i)
          val ends = endData(i)
          val durations = durationData(i)
          Data(clazz,values,unit,
            LocalDateTime.parse(starts,formatter),LocalDateTime.parse(ends,formatter),parseDuration(durations))
        }
      } catch {
        case e: Throwable => logger.warn(s"Can't parse data from ${Category.cats2String(clazz)}," +
          s" because ${e.getMessage}, skip now..."); Seq()
      }
    }
    val data =
      dataFor(Steps) ++ dataFor(VO2Max) ++ dataFor(WalkingRunningDistance) ++
        dataFor(FlightsClimbed) ++ dataFor(ActiveCalories) ++ dataFor(RestingCalories) ++
        dataFor(HeartRate) ++ dataFor(RestingHeartRate) ++ dataFor(WalkingHeartRateAverage) ++
        dataFor(HeartRateVariability) ++ dataFor(Breath) //++ dataFor(Weight)
    val res = if (skipZeroValueData) data.filter(_.value != 0) else data
    logger.info(s"Prepare ${res.length} data done."); res
  }
}

/////////////////////////////////// DAO  ///////////////////////////////////

trait FitnessComponent { self: HasDatabaseConfigProvider[JdbcProfile] =>
  import profile.api._

  implicit val catType =
    MappedColumnType.base[Category,String](Category.cats2String,Category.str2Cats)
  implicit val durationType =
    MappedColumnType.base[Duration,Long](_.getSeconds, Duration.ofSeconds)

  class Datas(tag:Tag) extends Table[Data](tag, "datas") {
    def category = column[Category]("category")
    def value = column[Double]("value")
    def unit = column[String]("unit")
    def start = column[LocalDateTime]("startTime")
    def end = column[LocalDateTime]("endTime")
    def duration = column[Duration]("duration")
    def id = column[Long]("id",O.PrimaryKey)
    override def * =
      (category, value, unit, start, end, duration, id) <> ((Data.apply _).tupled, Data.unapply)
  }

  lazy val datas = TableQuery[Datas]
  lazy val datasInsert = datas returning datas.map(_.id)
}

@Singleton
class FitnessRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider,
                                   val ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] with FitnessComponent {
  import profile.api._

  def all(category:Option[String],lastDays:Option[Int],
          durationBiggerSeconds:Option[Long],
          skipRow:Option[Long],limitRow:Option[Long]): Future[Seq[Data]] = {
    @inline def handledCategory(datas:TableQuery[Datas]): Option[Query[Datas, Data, Seq]] = {
      if (category.isEmpty) return Some(datas) //fetch all data
      Category.str2Cats(category.get) match {
        case null => None //some spell error
        case o => Some(datas.filter(_.category === o)) //filter some data
      }
    }
    @inline def handleDurationAndDays(datas:Query[Datas,Data,Seq]): Query[Datas, Data, Seq] = {
      val datasInternal = lastDays match {
        case None => datas
        case Some(d) => datas.filter(_.start >= LocalDateTime.now().minusDays(d))
      } //handle lastDay first
      durationBiggerSeconds match {
        case None => datasInternal
        case Some(dLimit) => datasInternal.filter(_.duration >= Duration.ofSeconds(dLimit))
      } //then handle durationBiggerSeconds
    }
    //provide default skip and limit value for performance
    val skied = skipRow.getOrElse(0L).toInt
    val limited = limitRow.getOrElse(1000L).toInt
    db.run {
      handledCategory(datas) match {
        case None => DBIO.successful(Seq())
        case Some(datasInternal) => handleDurationAndDays(datasInternal)
          .sortBy(_.start.desc).drop(skied).take(limited).result
      }
    }
  }

  /**
   * 一个批量插入，插入前先查询，但因为数据是按照小时分组的，因此插入时最后一个小时数据会随着不同的插入时间而改变
   * 因此存在少量的重复，需要通过一个额外的 Action 进行删除（按照 category 和 startTime 分组，然后获取其组数据
   * 大于 1 的最小 id，将其记录删除即可）。
   */
  @deprecated("此方法可能导致重复的数据插入，请使用 insertBatch2 方法与非自增主键进行插入","2.0.0")
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

  /**
   * 一个批量插入的实现，对插入的内容预设置 Id，当 Id 重复则进行更新，缺点：对于没有变化的数据如何处理？
   * 可能进行了不必要的刷新。这种实现需要主键自定义而非递增，但总的来说还是比上一个实现简单些。
   * Id 计算办法：使用 Category 和 StartTime 计算 HashCode。
   */
  def insertBatch2(js:JsValue): Future[Seq[Int]] = {
    val dta = Data.parseFieldJSON(js)
    //尤其注意，不要使用默认的 Case Class 的 hashCode 构建 id，因为其包含了 @2323 内存地址，每次重启 Play 会变
    val dtaWithId = dta.map(d => d.copy(id = (Category.cats2String(d.category) + d.start).hashCode))
    db.run {
      DBIO.sequence(
        dtaWithId.map(dWithId => datas.insertOrUpdate(dWithId)):Seq[DBIOAction[Int,NoStream,Effect.All]]
      )
    }
  }

  def schema: String = datas.schema.createStatements.mkString("\n") + "\n\n" +
               datas.schema.dropIfExistsStatements.mkString("\n")

}