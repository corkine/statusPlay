package services

import java.time.LocalDateTime

import com.google.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.libs.json._
import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

case class Address(hostName:String, lastIP:String, description:Option[String],
                   addTime:LocalDateTime = LocalDateTime.now(),id:Long = 0L)
object Address {
  implicit val aF: OFormat[Address] = Json.format[Address]
}

trait AddressComponent { self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class AddressTable(tag:Tag) extends Table[Address](tag, "addresses") {
    def hostName = column[String]("hostName")
    def lastIP = column[String]("lastIP")
    def description = column[Option[String]]("description")
    def addTime = column[LocalDateTime]("addTime")
    def id = column[Long]("id",O.PrimaryKey,O.AutoInc)
    def idx = index("addTimeIPIndex",(addTime, lastIP),unique = false)
    override def * =
      (hostName, lastIP, description, addTime, id) <> ((Address.apply _).tupled, Address.unapply)
  }

  lazy val addresses = TableQuery[AddressTable]
  lazy val addressesInsert = addresses returning addresses.map(_.id)
}

@Singleton
class AddressRepository @Inject() (protected val dbConfigProvider: DatabaseConfigProvider)
                                (implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] with AddressComponent {
  import profile.api._

  def allAddresses(lastDay:Option[Int],recentFirst:Boolean,skip:Long,take:Long): Future[Seq[Address]] =
    db.run(lastDay match {
      case None => addresses
        .sortBy(i => if (recentFirst) i.addTime.desc else i.addTime.asc)
        .drop(skip).take(take)
        .result
      case Some(day) => addresses
        .filter(_.addTime >= LocalDateTime.now().minusDays(day))
        .sortBy(i => if (recentFirst) i.addTime.desc else i.addTime.asc)
        .drop(skip).take(take)
        .result
    })

  def findHostNameAfter(hostName:Option[String],after:LocalDateTime): Future[Seq[Address]] = db.run {
    addresses.filter(_.addTime >= after)
      .filterOpt(hostName)(_.hostName like _)
      .sortBy(_.addTime.desc).result
  }

  def findHostNameIn(hostName:String, take:Option[Long], fullMatch:Boolean, days:Option[Long]): Future[Seq[Address]] = db.run {
    if (take.isEmpty) addresses.filterOpt(days)(_.addTime >= LocalDateTime.now().minusDays(_))
      .filter(_.hostName like (if (fullMatch) hostName else "%"+ hostName.trim + "%"))
      .sortBy(_.addTime.desc).result
    else addresses.filterOpt(days)(_.addTime >= LocalDateTime.now().minusDays(_))
      .filter(_.hostName like (if (fullMatch) hostName else "%"+ hostName.trim + "%"))
      .sortBy(_.addTime.desc).take(take.get).result
  }

  def findUsedIPIn(ip:String,days:Option[Long]): Future[Seq[Address]] = db.run {
    addresses.filterOpt(days)(_.addTime >= LocalDateTime.now().minusDays(_))
      .filter(_.lastIP === ip.trim)
      .sortBy(_.addTime.desc).result
  }

  def findHostNameOne(hostName:String): Future[Option[Address]] = db.run {
    addresses.filter(_.hostName === hostName.trim)
      .sortBy(_.addTime.desc).take(1).result.headOption
  }

  def deleteAllOfHostName(hostName:String): Future[Either[String,String]] = db.run {
    addresses.filter(_.hostName === hostName).delete
  }.map {
    case 0 => Left(s"Can't find this hostName $hostName")
    case o => Right(s"Delete $o rows done.")
  }

  def addRecord(record:Address): Future[Either[String,Address]] =
    db.run(addressesInsert += record)
      .map(fId => Right(record.copy(id = fId)))
      .recover {
        case e:Throwable => Left(s"Add Failure. ${e.getMessage}")
      }

  def schema: String = {
    val ws = addresses.schema.createStatements
    val wsd = addresses.schema.dropIfExistsStatements
    val sb = new StringBuilder
    sb.append(ws.mkString("\n"))
      .append("\n\n\n")
      .append(wsd.mkString("\n"))
      .append("\n\n\n")
      .toString()
  }
}
