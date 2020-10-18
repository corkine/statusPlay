package services

import java.time.LocalDateTime

import com.google.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

case class Food(name:String, picture:Option[String], description:Option[String], kind:Option[String],
                buyEatIntervalDay: Int = 0,
                evilDegree:Option[Int] = None,
                hungerDegree:Option[Int] = None,
                addTime:LocalDateTime = LocalDateTime.now(),
                finishTime:Option[LocalDateTime] = None,
                id:Long = 0L)

object Food {
  implicit val fF: Format[Food] =
    ((JsPath \ "name").format[String] and
      (JsPath \ "picture").formatNullable[String] and
      (JsPath \ "description").formatNullable[String] and
      (JsPath \ "kind").formatNullable[String] and
      (JsPath \ "buyEatIntervalDay").format[Int] and
      (JsPath \ "evilDegree").formatNullable[Int] and
      (JsPath \ "hungerDegree").formatNullable[Int] and
      (JsPath \ "addTime").format[LocalDateTime] and
      (JsPath \ "finishTime").formatNullable[LocalDateTime] and
      (JsPath \ "id").format[Long])(Food.apply, unlift(Food.unapply))
}

trait FoodsComponent { self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class FoodTable(tag:Tag) extends Table[Food](tag, "foods") {
    def name = column[String]("name")
    def picture = column[Option[String]]("picture")
    def description = column[Option[String]]("description")
    def kind = column[Option[String]]("kind")
    def buyEatIntervalDay = column[Int]("buyEatIntervalDay")
    def evilDegree = column[Option[Int]]("evilDegree")
    def hungerDegree = column[Option[Int]]("hungerDegree")
    def addTime = column[LocalDateTime]("addTime")
    def finishTime = column[Option[LocalDateTime]]("finishTime")
    def id = column[Long]("id",O.PrimaryKey,O.AutoInc)
    override def * =
      (name,picture,description,kind,buyEatIntervalDay,evilDegree,hungerDegree,
        addTime,finishTime,id) <> ((Food.apply _).tupled, Food.unapply)
  }

  lazy val foods = TableQuery[FoodTable]
  lazy val foodsInsert = foods returning foods.map(_.id)
}

@Singleton
class FoodsRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
                                (implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] with FoodsComponent {
  import profile.api._

  def allFoods(lastDay:Option[Int],recentFirst:Boolean,skip:Long,take:Long): Future[Seq[Food]] =
    db.run(lastDay match {
      case None => foods
        .sortBy(i => if (recentFirst) i.addTime.desc else i.addTime.asc)
        .drop(skip).take(take)
        .result
      case Some(day) => foods
        .filter(_.addTime >= LocalDateTime.now().minusDays(day))
        .sortBy(i => if (recentFirst) i.addTime.desc else i.addTime.asc)
        .drop(skip).take(take)
        .result
    })

  def rangeFoods(kind:Option[String],after:LocalDateTime): Future[Seq[Food]] = db.run {
    kind match {
      case None => (foods.filter(_.addTime >= after) union
        foods.filter(_.finishTime >= after)).sortBy(_.addTime.desc).result
      case Some(k) =>
        val ks = s"%$k%"
        (foods.filter(_.addTime >= after) union
        foods.filter(_.finishTime >= after)).filter(_.kind like ks).sortBy(_.addTime.desc).result
    }
  }

  def singleFood(foodId:Long): Future[Either[String,Food]] = db.run {
    foods.filter(_.id === foodId).result.headOption
  }.map(_.toRight(s"Can't find this foodId $foodId."))

  def findFood(oKey:String, addInRecentDay:Option[Int]): Future[Seq[Food]] = db.run {
    val key = s"%$oKey%"
    addInRecentDay match {
      case None => (foods.filter(_.name like key) union foods.filter(_.description like key)).result
      case Some(d) => (foods.filter(_.name like key) union foods.filter(_.description like key))
        .filter(_.addTime >= LocalDateTime.now().minusDays(d))
        .sortBy(_.addTime.desc).result
    }
  }

  def deleteFood(foodId:Long): Future[Either[String,String]] = db.run {
    foods.filter(_.id === foodId).delete
  }.map {
    case 0 => Left(s"Can't find this id $foodId")
    case o => Right(s"Delete $o rows done.")
  }

  def addFoods(food:Food): Future[Either[String,Food]] =
    db.run(foodsInsert += food)
      .map(fId => Right(food.copy(id = fId)))
      .recover {
        case e:Throwable => Left(s"Add Failure. ${e.getMessage}")
      }

  def updateFood(foodId:Long, food:Food): Future[Either[String, Food]] =
    db.run(foods.filter(_.id === foodId).update(food))
      .map {
        case 0 => Left(s"Can't update this food $foodId")
        case _ => Right(food)
      }

  def schema: String = {
    val ws = foods.schema.createStatements
    val wsd = foods.schema.dropIfExistsStatements
    val sb = new StringBuilder
    sb.append(ws.mkString("\n"))
      .append("\n\n\n")
      .append(wsd.mkString("\n"))
      .append("\n\n\n")
      .toString()
  }
}
