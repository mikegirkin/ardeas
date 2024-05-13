#!/usr/bin/env sh

sbt ";run codecs ./src/test/resources/petstore_complete.yaml circe_scala2 ../ardeas_test/src/main/scala/sttp/codecs.scala --package org.petstore.sttp \
     ;run entities ./src/test/resources/petstore_complete.yaml scala ../ardeas_test/src/main/scala/sttp/entities.scala --package org.petstore.sttp \
     ;run client ./src/test/resources/petstore_complete.yaml sttp3_scala2 ../ardeas_test/src/main/scala/sttp/client.scala --package org.petstore.sttp --additionalImport CirceInterop._ --additionalImport Codecs._"


