# Ardeas

This is a client and service skeleton code generator from OpenAPI specs

## Limitations at the moment:

- no support for anonymous objects anywhere
- no support for OpenAPI auth specification
- no support for parameters in `components`
- no support for `oneOf` and `allOf` constructs
- only supports `application/json` responses media type

# Current generators

- Scala entities for `components/schema` and type aliases for `components/responses` and `components/requestBodies`
- JSON codecs using `spray` and `circe` libs
- `http4s` service skeleton with an abstract effect (ie. non-binding to `cats.effect`)
- `http4s`-based client, with an abstract effect

# CLI

## Entities

```sh
sbt "run entities ./src/test/resources/petstore.yaml scala ./entities.scala --package org.petstore"
```

## Codecs

```sh
sbt "run codecs ./src/test/resources/petstore.yaml spray ./codecs.scala"
```

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

```shell
sbt "run client ./src/test/resources/petstore_complete.yaml http4s_scala2 ../client.scala \
  --package net.girkin.petstore.complete.client
  --additionalImport org.http4s.circe.CirceEntityCodec._ \
  --additionalImport net.girkin.petstore.complete.client.Codecs._"
```

# Assumptions
- No anonymous types
- Only Json media type
- path templating only works for when parameters are split with "/"
- path parameters must be required
- models & codecs & (service | client) must sit in the same package

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
- [ ] Create directory when generating files
- [ ] Check if optional parameter work
- [ ] Improve error messaging in CLI
- [ ] Support OneOf declaration
- [ ] Description to go into comments
- [ ] Support 1st level anonymous schemas for names Requests and Responses 
- [ ] Support no content responses
- [ ] Handle parameters from components 
- [ ] Handle enums in schemas
- [ ] Ability to provide serialization engine different than circe for http4s
- [ ] Simplified method `makeRoutes` for service with the default value for preProcessHeaders
- [ ] Simplified interface for service with no access to raw request
- [ ] Handle octet-stream mediatype
- [ ] Server: Custom error handling for request parsing errors
- [ ] Server: Custom error handling for query parameters parsing errors
- [ ] Obey parameter spec style/explode definition in openapi spec
- [ ] Refactor spray codec renderer to use stricter Model types
- [ ] Imports for spray codecs
- [ ] ...


## Http4s Client
- [x] Pass entity body in the requests
- [x] Codec interop
- [x] Pass underlying http4s response to the caller
