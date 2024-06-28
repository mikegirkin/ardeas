package io.github.mikegirkin.ardeas

import org.slf4j.LoggerFactory

trait Logging:
  protected val logger = LoggerFactory.getLogger(this.getClass)
