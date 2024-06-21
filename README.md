# Ardeas

*Ardeas* (Greek Ἀρδείας) - son of Odysseus and Circe

This is a client and service skeleton code generator from OpenAPI specs

## Limitations at the moment:

- no support for inline schema definitions anywhere
- no support for OpenAPI auth specification
- no support for parameters in `components`
- Only `oneOf` construct containing refs is supported
- no support for `allOf` and `anyOf` constructs
- only supports `application/json` responses media type
- all generators assume Scala 2.13 as a target language
- path templating only works for when parameters are split with "/"
- no trailing slash expected in the paths


# Current generators

- Scala entities for `components/schema` and type aliases for `components/responses` and `components/requestBodies`
- JSON codecs using `spray` and `circe` libs
- `http4s` service skeleton with an abstract effect (ie. non-binding to `cats.effect`)
- `http4s`-based client, with an abstract effect
- `sttp3`-based client

# CLI

## Entities

```sh
sbt "run entities ./src/test/resources/petstore.yaml scala ./entities.scala --package org.petstore"
```

## Codecs

### Spray

```sh
sbt "run codecs ./src/test/resources/petstore.yaml spray ./codecs.scala"
```

### Circe

```sh
sbt "run codecs ./src/test/resources/petstore.yaml circe_scala2 ./codecs.scala --package org.petstore"
```

## Service

```shell
sbt "run service ./src/test/resources/petstore_complete.yaml http4s_scala2 ../service.scala \
  --package net.girkin.petstore.complete.service \
  --additionalImport org.http4s.circe.CirceEntityCodec._ \
  --additionalImport net.girkin.petstore.complete.service.Codecs._"
```

## Client

### http4s

```shell
sbt "run client ./src/test/resources/petstore_complete.yaml http4s_scala2 ../client.scala \
  --package net.girkin.petstore.complete.client
  --additionalImport org.http4s.circe.CirceEntityCodec._ \
  --additionalImport net.girkin.petstore.complete.client.Codecs._"
```

### sttp3

```shell
sbt "run client ./src/test/resources/petstore_complete.yaml sttp3_scala2 ../client.scala \
  --package net.girkin.petstore.complete.client \
  --additionalImport net.girkin.petstore.complete.client.Codecs._"
```

The client generated assumes that there is 2 functions defined and in the scope

```scala
  def deserialize[T](str: String): Either[Error, T]
  def serialize[T](entity: T): String
```

When using `Circe` for json serialization, this could be achieved by putting this into the project:

```scala
package util

import io.circe._

object CirceInterop {
  def deserialize[T: Decoder](str: String): Either[Error, T] = {
    parser.decode(str)
  }

  def serialize[T: Encoder](entity: T): String = {
    implicitly[Encoder[T]].apply(entity).toString()
  }
}
```

And then adding `additionalImport` parameter to the client generation starter, like this:

```shell
sbt "run client ./src/test/resources/petstore_complete.yaml sttp3_scala2 ../client.scala \
  --package net.girkin.petstore.complete.client \
  --additionalImport util.CirceInterop_ \
  --additionalImport net.girkin.petstore.complete.client.Codecs._"
```


# TODO

- [x] Render entities as CLI
- [x] Render codecs as CLI
- [x] Circe codec renderer
- [x] Run circe codec renderer from CLI
- [x] Codecs must be in objects
- [x] Imports for circe codecs
- [x] Implicit variables must have explicit type
- [x] Fix the issue with returning responses, they must me Response[F]
- [x] Minimal starting Http4s service
- [x] Request body
- [x] Query parameters
- [x] Typed query parameters
- [x] Custom package name for renderings
- [x] Render entities in Components.Schemas
- [x] Query parameters could have same names - would result in compile error
- [x] Remove entities render from service
- [x] Render http4s <=> circe interop
- [x] Typed path parameters
- [x] Reject non-required path parameters when parsing
- [x] Handle exploding lists in query parameters
- [x] Handle additionalProperties in objects
- [x] Incoming headers handling
- [x] Outgoing headers handling
- [x] Service: raw request exposure
- [x] Client: raw response exposure
- [x] Handle requestBodies from components
- [x] Optional request bodies defined in components
- [x] Change how we render encoders/decoders for http4s
- [x] FIX: Circe interop looks weird as it renders codecs for `Vector[OtherType]` and alike
- [x] Ability to set headers for client
- [x] Handle responses from components
- [x] Support additionalImport in CLI
- [x] Introduce client interface
- [x] Create directory when generating files
- [x] Imports for spray codecs
- [x] Fix spray optional codecs (petstore_ext)
- [x] sttp3 client generator
- [x] Handle string enums in schemas
- [ ] support OpenAPI 3.1 - parsing has changed between 3.0 and 3.1
- [ ] pekko client generator contains reference to http4s in HTTP 405 response
- [ ] duplicated code in client generators requires refactoring
- [ ] Support "noDefaultImports" CLI option
- [ ] Support escapes for internal variables in generated client/service to prevent name clashes with parameters
- [ ] Allow option to treat inline schemas content as string or byte array
- [ ] operationId might contain wierd characters, handle with backticks 
- [ ] Support OneOf declaration
  - [ ] without discriminator
    - [x] circe
    - [ ] spray
  - [ ] with discriminator
    - [x] circe
    - [ ] spray
- [ ] Support enums
- [ ] Support trailing slash in paths
- [ ] Move packages rendering from RenderUtils to ScalaSpecifics
- [ ] Url-encode parameters passed in the path in client
- [ ] Url-decode parameters passed in the path in server (check if required) 
- [ ] Support headers in the responses
- [ ] Improve error messaging in CLI
- [ ] Packaging and release as uberjar
- [ ] Check if optional parameter work
- [ ] Description to go into comments
- [ ] Support 1st level anonymous schemas for names Requests and Responses 
- [ ] Support no content responses
- [ ] Handle parameters from components 
- [ ] Ability to provide serialization engine different than circe for http4s
- [ ] Simplified method `makeRoutes` for service with the default value for preProcessHeaders
- [ ] Simplified interface for service with no access to raw request
- [ ] Handle octet-stream mediatype
- [ ] Server: Custom error handling for request parsing errors
- [ ] Server: Custom error handling for query parameters parsing errors
- [ ] Obey parameter spec style/explode definition in openapi spec
- [ ] Refactor spray codec renderer to use stricter Model types
- [ ] ...


## Http4s Client
- [x] Pass entity body in the requests
- [x] Codec interop
- [x] Pass underlying http4s response to the caller
