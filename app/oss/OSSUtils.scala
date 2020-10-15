package oss

import java.io.{File, PrintWriter, StringWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util

import com.aliyun.oss.model.OSSObjectSummary
import com.google.inject.{Inject, Singleton}
import org.slf4j.LoggerFactory
import play.api.Configuration
import java.util.UUID

import com.aliyun.oss.OSSClient
import com.aliyun.oss.model.ListObjectsRequest
import com.aliyun.oss.model.GetObjectRequest
import com.aliyun.oss.model.{CannedAccessControlList, CreateBucketRequest, PutObjectRequest}
import com.typesafe.config.Config


@Singleton
class OSSUtils @Inject()(playConfig:Configuration) {

  private val logger = LoggerFactory.getLogger(classOf[OSSUtils])

  val config: Config = playConfig.get[Config]("aliyun")
  val bucketName: String = config.getString("oss.bucketName")
  val fileHost: String = config.getString("oss.fileHost")
  def newClient = new OSSClient(config.getString("oss.endpoint"),
      config.getString("oss.accessKeyId"),
      config.getString("oss.accessKeySecret"))

  def upload(file: File):String = {
    val client = newClient
    val time = LocalDate.now().format(DateTimeFormatter.BASIC_ISO_DATE)
    val filePath = s"goods/$time/${UUID.randomUUID().toString.replace("-","").substring(0,7)}"
    val fileUrl = filePath + s"_${file.getName.replace(" ","")}"

    try {
      logger.info(s"OSS Start Uploading, File:${file.getName}")

      if (!client.doesBucketExist(bucketName)) {
        client.createBucket(bucketName)
        val request = new CreateBucketRequest(bucketName)
        request.setCannedACL(CannedAccessControlList.PublicRead)
        client.createBucket(request)
      }
      val putRequest = new PutObjectRequest(bucketName, fileUrl, file)
      val putResult = client.putObject(putRequest)
      client.setBucketAcl(bucketName, CannedAccessControlList.PublicRead)
      putResult match {
        case null => logger.info(s"$file - $fileUrl - File upload failed - $putResult")
        case _ =>
          logger.info(s"$file - $fileUrl - Upload done @ ${fileHost + "/" + fileUrl}.")
          return fileHost + "/" + fileUrl
      }
    } catch {
      case ex : Exception =>
        val w = new StringWriter()
        ex.printStackTrace(new PrintWriter(w))
        logger.info(ex.getMessage + ", Details: \n" + w.toString)
    } finally {
      if (client != null) client.shutdown()
    }
    null
  }

  def downloadFile(objectName: String, localFileName: String): Boolean = {
    try {
      val ossClient = newClient
      ossClient.getObject(new GetObjectRequest(bucketName, objectName), new File(localFileName))
      ossClient.shutdown()
      return true
    } catch {
      case ex :Exception =>
        logger.info(s"DownLoad File Error: ${ex.getMessage}")
    }
    false
  }

  def listFile(prefix:String = ""): util.List[OSSObjectSummary] = {
    val ossClient = newClient
    val listObjectsRequest = new ListObjectsRequest(bucketName)
    if (prefix != "") listObjectsRequest.setPrefix(prefix)
    val listing = ossClient.listObjects(listObjectsRequest)

    val listWithOutFolders = listing.getObjectSummaries
    ossClient.shutdown()
    listWithOutFolders
  }

  def listFolder(prefix:String = ""): util.List[String] = {
    val ossClient = newClient
    val listObjectsRequest = new ListObjectsRequest(bucketName)
    if (prefix != "") listObjectsRequest.setPrefix(prefix)
    val listing = ossClient.listObjects(listObjectsRequest)

    val listJustFolders = listing.getCommonPrefixes
    ossClient.shutdown()
    listJustFolders
  }
}
