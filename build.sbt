import scala.concurrent.duration.DurationInt

ThisBuild / scalaVersion := "3.3.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "io.adamnfish"
ThisBuild / organizationName := "adamnfish"

ThisBuild / scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-encoding",
  "UTF-8",
  "-deprecation",
  "-java-output-version",
  "21",
  // avoid a scanamo derivation error message
  "-Xmax-inlines",
  "64"
)

val circeVersion = "0.14.16"
val scanamoVersion = "7.0.0"
val awsJavaSdkVersion = "2.48.3"
val commonDeps = Seq(
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
  "org.scalameta" %% "munit" % "1.3.4" % Test,
  "org.scalameta" %% "munit-scalacheck" % "1.3.0" % Test,
  "org.typelevel" %% "scalacheck-effect-munit" % "2.1.0" % Test,
  "org.typelevel" %% "munit-cats-effect" % "2.2.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.19.0" % Test,
  "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test
)
val loggingDeps = Seq(
  "org.typelevel" %% "log4cats-slf4j" % "2.8.0",
  "ch.qos.logback" % "logback-classic" % "1.5.38",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6"
)

// https://aws.amazon.com/blogs/developer/tuning-the-aws-java-sdk-2-x-to-reduce-startup-time/
// url-connection-client is included in modules that make AWS API calls (lambda, devserver and integration)
// some other jars are also filtered out of the Lambda in its native packager settings
ThisBuild / excludeDependencies ++= Seq(
  ExclusionRule("software.amazon.awssdk", "netty-nio-client"),
  ExclusionRule("software.amazon.awssdk", "apache-client")
)

lazy val root = (project in file("."))
  .settings(
    name := "pokerdot",
    libraryDependencies ++= commonDeps
  )
  .aggregate(core, lambda, devServer, integration)

lazy val core = (project in file("core"))
  .settings(
    name := "core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-effect" % "3.7.0",
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "software.amazon.awssdk" % "dynamodb" % awsJavaSdkVersion,
      "org.scanamo" %% "scanamo" % scanamoVersion,
      "org.scanamo" %% "scanamo-cats-effect" % scanamoVersion
    ) ++ commonDeps
  )

lazy val lambda = (project in file("lambda"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "lambda",
    libraryDependencies ++= Seq(
      "com.amazonaws" % "aws-lambda-java-core" % "1.4.0",
      "com.amazonaws" % "aws-lambda-java-events" % "3.16.1",
      "com.amazonaws" % "aws-xray-recorder-sdk-core" % "2.21.1",
      "software.amazon.awssdk" % "apigatewaymanagementapi" % awsJavaSdkVersion,
      // TODO: use the async crt version for everything
      "software.amazon.awssdk" % "url-connection-client" % awsJavaSdkVersion,
      "software.amazon.awssdk" % "aws-crt-client" % awsJavaSdkVersion
    ) ++ commonDeps ++ loggingDeps,
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
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.8.0" % Test,
      // TODO: use the async crt version for everything
      "software.amazon.awssdk" % "url-connection-client" % awsJavaSdkVersion % Test,
      "software.amazon.awssdk" % "aws-crt-client" % awsJavaSdkVersion % Test,
      "software.amazon.awssdk" % "dynamodb" % awsJavaSdkVersion % Test,
      "org.scanamo" %% "scanamo-testkit" % scanamoVersion % Test
    ) ++ commonDeps ++ loggingDeps,
    scalacOptions ++= Seq(
      "-source",
      "future"
    ),
    // start DynamoDB for tests
    dynamoDBLocalDownloadDir := file(".dynamodb-local"),
    dynamoDBLocalPort := 8042,
    dynamoDBLocalDownloadIfOlderThan := 14.days,
    startDynamoDBLocal := startDynamoDBLocal.dependsOn(Test / compile).value,
    Test / test := (Test / test).dependsOn(startDynamoDBLocal).value,
    Test / testOnly := (Test / testOnly)
      .dependsOn(startDynamoDBLocal)
      .evaluated,
    Test / testOptions += dynamoDBLocalTestCleanup.value
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val devServer = (project in file("devserver"))
  .settings(
    name := "devserver",
    libraryDependencies ++= Seq(
      "io.javalin" % "javalin" % "7.2.2",
      "software.amazon.awssdk" % "dynamodb" % awsJavaSdkVersion,
      // TODO: use the async crt version for everything
      "software.amazon.awssdk" % "url-connection-client" % awsJavaSdkVersion,
      "software.amazon.awssdk" % "aws-crt-client" % awsJavaSdkVersion,
      "org.scanamo" %% "scanamo-testkit" % scanamoVersion
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
    dynamoDBLocalSharedDB := true
  )
  .dependsOn(core)
