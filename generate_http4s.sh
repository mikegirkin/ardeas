#!/usr/bin/env sh

sbt ";run codecs ./src/test/resources/petstore_ext.yaml circe_scala2 ../ardeas_test/src/main/scala/http4s/codecs.scala --package org.petstore.http4s \
     ;run entities ./src/test/resources/petstore_ext.yaml scala ../ardeas_test/src/main/scala/http4s/entities.scala --package org.petstore.http4s \
     ;run client ./src/test/resources/petstore_ext.yaml http4s_scala2 ../ardeas_test/src/main/scala/http4s/client.scala --package org.petstore.http4s"

