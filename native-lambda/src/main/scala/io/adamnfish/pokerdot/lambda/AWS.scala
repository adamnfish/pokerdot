package io.adamnfish.pokerdot.lambda

case class RequestIdentity(
  apiKey: Option[String],
  userArn: Option[String],
  cognitoAuthenticationType: Option[String],
  caller: Option[String],
  userAgent: Option[String],
  user: Option[String],
  cognitoIdentityPoolId: Option[String],
  cognitoAuthenticationProvider: Option[String],
  sourceIp: Option[String],
  accountId: Option[String],
)

case class RequestContext(
  resourceId: String,
  apiId: String,
  resourcePath: String,
  httpMethod: String,
  accountId: String,
  stage: String,
  identity: RequestIdentity,
  extendedRequestId: Option[String],
  path: String
)

// The request returned from the next-event url
case class RequestEvent(
  httpMethod: String,
  body: Option[String],
  resource: String,
  requestContext: RequestContext,
  queryStringParameters: Option[Map[String, String]],
  headers: Option[Map[String, String]],
  pathParameters: Option[Map[String, String]],
  stageVariables: Option[Map[String, String]],
  path: String,
  isBase64Encoded: Boolean
)

case class LambdaResponse(
  statusCode: String,
  headers: Map[String, String],
  body: String,
  isBase64Encoded: Boolean = false
)
