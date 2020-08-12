package tasks

import java.time.LocalDateTime

import akka.actor.ActorSystem
import javax.inject.Inject
import play.api.Logger
import play.api.inject.{SimpleModule, _}
import play.api.libs.concurrent.CustomExecutionContext
import play.api.libs.ws.WSClient
import services.{ActivityRepository, Website}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class TestingCheckTask @Inject() (actorSystem: ActorSystem, val ws: WSClient, val ac:ActivityRepository,
                                  implicit val ec: TasksCustomExecutionContext) extends TaskComponent {
  val sleepDuration: FiniteDuration = 30.minutes //每次全部查询休息时间
  val pageTimeoutDuration: FiniteDuration = 10000.millis //每个 website 页面超时
  actorSystem.scheduler.scheduleWithFixedDelay(initialDelay = 10.seconds, delay = sleepDuration) { () =>
    actorSystem.log.info("Executing now...")
    fetchRunInsert()
  }
}

trait TaskComponent {
  implicit val ec: CustomExecutionContext
  val logger: Logger = Logger.apply(this.getClass)

  val sleepDuration: FiniteDuration
  val pageTimeoutDuration: FiniteDuration
  val ws: WSClient
  val ac: ActivityRepository

  def fetchRunInsert(): Unit = {
    logger.info("Starting fetchRunInsert data... " + Thread.currentThread())
    val e = ac.allWebsites
      .flatMap(all => Future.sequence(all.map(handleWebsite)))
      .flatMap(s => ac.logBatchActivity(s))
    val result = Await.result(e, sleepDuration)
    logger.info(s"Batch Data done with insert result activity id: ${result.mkString(", ")} " + Thread.currentThread())
  }

  private def handleWebsite(website: Website) = {
    logger.info("fullUrl " + fullUrl(website.url) + " by " +  Thread.currentThread())
    ws.url(fullUrl(website.url)).withRequestTimeout(pageTimeoutDuration).get().map { rep =>
      //logger.info("Now by "  + Thread.currentThread())
      if (rep.status == 200) services.Ok
      else services.UnStable
    } recover {
      case _ => services.Offline
    } map { s => (website, s, Some(s"Fetch @${LocalDateTime.now()} by Akka.")) }
  }

  private def fullUrl(in:String) = {
    if (in.startsWith("http://") || in.startsWith("https://")) in
    else "http://" + in
  }
}

class TasksCustomExecutionContext @Inject() (actorSystem: ActorSystem)
  extends CustomExecutionContext(actorSystem, "tasks.executor")

class TestingCheckModel extends SimpleModule(bind[TestingCheckTask].toSelf.eagerly())
