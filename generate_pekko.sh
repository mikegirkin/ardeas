#!/usr/bin/env sh

sbt ";run codecs ./src/test/resources/petstore_ext.yaml circe_scala2 ../ardeas_test/src/main/scala/pekko/codecs.scala --package org.petstore.pekko \
     ;run entities ./src/test/resources/petstore_ext.yaml scala ../ardeas_test/src/main/scala/pekko/entities.scala --package org.petstore.pekko \
     ;run client ./src/test/resources/petstore_ext.yaml pekko_scala2 ../ardeas_test/src/main/scala/pekko/client.scala --package org.petstore.pekko"

