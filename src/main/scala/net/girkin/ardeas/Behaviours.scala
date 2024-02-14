package net.girkin.ardeas

object Behaviours:


  def typeNameForResponseEntity(verb: Model.HttpVerb, operationId: String, httpCode: Int | Model.Default.type): String =
    s"${verb}${operationId}Http${httpCode}ResponseEntity"

