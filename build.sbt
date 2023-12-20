import scala.concurrent.duration.DurationInt

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.adamnfish"
ThisBuild / organizationName := "adamnfish"

ThisBuild / scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-encoding", "UTF-8",
  "-Ywarn-dead-code",
  "-deprecation",
  "-explaintypes",
)


val circeVersion = "0.14.5"
val scanamoVersion = "1.0-M14"
val awsJavaSdkVersion = "2.20.68"
val commonDeps = Seq(
  "org.scalatest" %% "scalatest" % "3.2.15" % Test,
  "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test,
)
val loggingDeps = Seq(
  "ch.qos.logback" % "logback-classic" % "1.4.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
)

// https://aws.amazon.com/blogs/developer/tuning-the-aws-java-sdk-2-x-to-reduce-startup-time/
// url-connection-client is included in modules that make AWS API calls (lambda, devserver and integration)
// some other jars are also filtered out of the Lambda in its native packager settings
ThisBuild / excludeDependencies ++= Seq(
  ExclusionRule("software.amazon.awssdk", "netty-nio-client"),
  ExclusionRule("software.amazon.awssdk", "apache-client"),
)

lazy val root = (project in file("."))
  .settings(
    name := "pokerdot",
    libraryDependencies ++= commonDeps,
  )
  .aggregate(core, lambda, devServer, integration)

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.13",
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.scanamo" %% "scanamo" % scanamoVersion,
      "software.amazon.awssdk" % "dynamodb" % awsJavaSdkVersion,
    ) ++ commonDeps,
  )

lazy val lambda = (project in file("lambda"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "lambda",
    libraryDependencies ++= Seq(
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "com.amazonaws" % "aws-lambda-java-core" % "1.2.2",
      "com.amazonaws" % "aws-lambda-java-events" % "3.11.1",
      "software.amazon.awssdk" % "apigatewaymanagementapi" % awsJavaSdkVersion,
      "software.amazon.awssdk" % "url-connection-client" % awsJavaSdkVersion,
    ) ++ commonDeps,
    // native-packager
    Universal / topLevelDirectory := None,
    Universal / packageName := "pokerdot-lambda",
    Compile / packageDoc / mappings := Seq(),
    Universal / mappings := (Universal / mappings).value.filter {
      case (_, path) =>
        // these are only used at compile time to generate code, I think?
//          !path.contains("org.scala-lang.scala-compiler") && // required :-(
//          !path.contains("org.scala-lang.scala-reflect") && // required :-(
          !path.contains("net.java.dev.jna.jna") &&
          !path.contains("org.jline.jline")
    }
  )
  .dependsOn(core)

lazy val integration = (project in file("integration"))
  .settings(
    name := "integration",
    libraryDependencies ++= Seq(
      "org.scanamo" %% "scanamo-testkit" % scanamoVersion % Test,
      "software.amazon.awssdk" % "url-connection-client" % awsJavaSdkVersion % Test,
      "software.amazon.awssdk" % "dynamodb" % awsJavaSdkVersion % Test,
    ) ++ commonDeps ++ loggingDeps,
    // start DynamoDB for tests
    dynamoDBLocalDownloadDir := file(".dynamodb-local"),
    dynamoDBLocalPort := 8042,
    dynamoDBLocalDownloadIfOlderThan := 14.days,
    startDynamoDBLocal := startDynamoDBLocal.dependsOn(Test / compile).value,
    Test / test := (Test / test).dependsOn(startDynamoDBLocal).value,
    Test / testOnly := (Test / testOnly).dependsOn(startDynamoDBLocal).evaluated,
    Test / testOptions += dynamoDBLocalTestCleanup.value,
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val devServer = (project in file("devserver"))
  .settings(
    name := "devserver",
    libraryDependencies ++= Seq(
      "io.javalin" % "javalin" % "5.6.3",
      "org.scanamo" %% "scanamo-testkit" % scanamoVersion,
      "software.amazon.awssdk" % "dynamodb" % awsJavaSdkVersion,
      "software.amazon.awssdk" % "url-connection-client" % awsJavaSdkVersion,
    ) ++ commonDeps ++ loggingDeps,
    // console logging and ctrl-c to kill support
    run / fork := true,
    run / connectInput := true,
    outputStrategy := Some(StdoutOutput),
    // start DynamoDB on run
    dynamoDBLocalDownloadDir := file(".dynamodb-local"),
    dynamoDBLocalPort := 8042,
    dynamoDBLocalDownloadIfOlderThan := 14.days,
    startDynamoDBLocal := startDynamoDBLocal.dependsOn(Compile / compile).value,
    Compile / run := (Compile / run).dependsOn(startDynamoDBLocal).evaluated,
    // allows browsing DB from http://localhost:8042/shell/
    dynamoDBLocalSharedDB := true,
  )
  .dependsOn(core)
