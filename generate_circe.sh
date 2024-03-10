#!/usr/bin/env sh

sbt ";run codecs ./src/test/resources/petstore_ext.yaml circe_scala2 ../ardeas_test/src/main/scala/circe/codecs.scala --package org.petstore.circe \
     ;run entities ./src/test/resources/petstore_ext.yaml scala ../ardeas_test/src/main/scala/circe/entities.scala --package org.petstore.circe"

