package io.adamnfish.pokerdot

import io.adamnfish.pokerdot.lambda.{LambdaResponse, RequestEvent}
import io.adamnfish.pokerdot.lambda.Serialisation._
import io.circe.parser.parse
import io.circe.syntax._


object Main {
  def main(args: Array[String]): Unit = {
    val runtimeApiHost = sys.env("AWS_LAMBDA_RUNTIME_API")
    handleEvents(runtimeApiHost)
  }

  // Receive and handle events infinitely
  def handleEvents(runtimeApiHost: String): Unit = {
    val url = nextEventUrl(runtimeApiHost)
    while (true) {
      val r = requests.get(url)
      val statusCode = for {
        jsonStr <- parse(r.text()).left.map(_.message)
        requestEvent <- jsonStr.as[RequestEvent].left.map(_.message)
        deadlineMs <- getRequiredHeader(r, "lambda-runtime-deadline-ms").map(_.toLong)
        requestId <- getRequiredHeader(r, "lambda-runtime-aws-request-id")
        statusCode = handleRequest(runtimeApiHost, requestEvent, requestId, deadlineMs)
      } yield statusCode

      statusCode.fold(
        { err =>
          Console.err.println(s"Could not handle event $err: $r")
        },
        { statusCode =>
          Console.err.println(s"[INFO] request complete $statusCode")
        }
      )
    }
  }

  // Retrieve the specified header from the response. If not available, report the header as missing
  def getRequiredHeader(nextEventResponse: requests.Response, header: String): Either[String, String] = {
    nextEventResponse.headers.getOrElse(header, Seq.empty).headOption.toRight {
      s"Request did not include required header `$header``"
    }
  }

  // Handle a valid request to this function
  def handleRequest(host: String, requestEvent: RequestEvent, requestId: String, deadlineMs: Long): Int = {
      val response = LambdaResponse("200", Map("Content-Type" -> "text/plain"), s"Hello, world!")
      val json = response.asJson.noSpaces
      requests.post(responseUrl(host, requestId), data = json).statusCode
  }

  def initializationErrorUrl(host: String) =
    s"http://$host/2018-06-01/runtime/init/error"

  // The url used to retrieve the next function request
  def nextEventUrl(host: String) =
    s"http://$host/2018-06-01/runtime/invocation/next"

  // The url used to write a response back to the caller
  def responseUrl(host: String, requestId: String) =
    s"http://$host/2018-06-01/runtime/invocation/$requestId/response"

  // The url used to write a error message to AWS
  def invocationErrorUrl(host: String, requestId: String) =
    s"http://$host/2018-06-01/runtime/invocation/$requestId/error"
}
