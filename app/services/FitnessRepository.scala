package services

import java.time.LocalDateTime

import com.google.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

case class Record(day:LocalDateTime, calorie:Int, note:Option[String], id:Long)
object Record {
  implicit val rf: Format[Record] = (
    (JsPath \ "day").format[LocalDateTime] and
      (JsPath \ "calorie").format[Int] and
      (JsPath \ "note").formatNullable[String] and
      (JsPath \ "id").format[Long])(Record.apply, unlift(Record.unapply))
}

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
}

@Singleton
class FitnessRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
                                   (implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] with FitnessComponent {
  import profile.api._

  def all(from:LocalDateTime,to:LocalDateTime): Future[Seq[Record]] = db.run {
    records.filter(o => o.day >= from && o.day <= to).result
  }

  def recordDetails(recordId:Long): Future[Option[Record]] = db.run {
    records.filter(_.id === recordId).result.headOption
  }

  def schema = records.schema.createStatements.mkString("\n") + "\n\n" +
               records.schema.dropIfExistsStatements.mkString("\n")

}