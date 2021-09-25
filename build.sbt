name := "statusPlay"
 
version := "2.10.13"
      
lazy val `statusplay` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "https://repo.akka.io/snapshots/"
      
scalaVersion := "2.12.2"

libraryDependencies ++= Seq(caffeine , ws , specs2 % Test , guice )

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-slick" % "5.0.0",
  "com.typesafe.play" %% "play-slick-evolutions" % "5.0.0",
  "com.h2database" % "h2" % "1.4.199",
  "com.aliyun.oss" % "aliyun-sdk-oss" % "2.8.3"
)


unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )

assemblyMergeStrategy in assembly := {
    case PathList("org", "apache", "commons", "logging", xs @ _*) => MergeStrategy.first
    case manifest if manifest.contains("MANIFEST.MF") =>
      MergeStrategy.discard
    case moduleInfo if moduleInfo.contains("module-info.class") =>
      MergeStrategy.discard
    case referenceOverrides if referenceOverrides.contains("reference-overrides.conf") =>
      MergeStrategy.concat
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
}



      