#!/usr/bin/env sh

sbt ";run codecs ./src/test/resources/petstore_ext.yaml spray ../ardeas_test/src/main/scala/spray/codecs.scala --package org.petstore \
     ;run entities ./src/test/resources/petstore_ext.yaml scala ../ardeas_test/src/main/scala/spray/entities.scala --package org.petstore"

