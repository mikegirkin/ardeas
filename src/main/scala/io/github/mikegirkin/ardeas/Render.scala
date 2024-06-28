package io.github.mikegirkin.ardeas

trait Render {
  val lineSeparator = System.lineSeparator()
  val doubleLineSeparator = System.lineSeparator() + System.lineSeparator()

  def indent(width: Int, prefix: String = "", separator: String = lineSeparator, suffix: String = "")(str: String*): String =
    str.map {
      _.split(lineSeparator)
        .map { line =>
          if line.nonEmpty then
            List.fill(width)(" ").mkString + line
          else
            line
        }
        .mkString(lineSeparator)
    }.mkStringIfNonEmpty(prefix, separator, suffix)

  def capitalizeFirst(str: String) = {
    val firstSymbol = str.take(1).capitalize
    s"$firstSymbol${str.drop(1)}"
  }

  def lowercaseFirst(str: String) = {
    val firstSymbol = str.take(1).toLowerCase
    s"$firstSymbol${str.drop(1)}"
  }

  def packageClause(packageName: String) = {
    s"package $packageName"
  }

  def importsClause(packagesToImport: Iterable[String]) = {
    packagesToImport.map { packageName =>
      s"import $packageName"
    }.mkString(lineSeparator)
  }

  def packageAndImportsHeader(`package`: Option[String], importPackages: Iterable[String]): Option[String] = {
    val packageClause = `package`.map(this.packageClause)
    val importsClause =
      if(importPackages.isEmpty) None
      else Some(this.importsClause(importPackages))

    val preRendered = List(
      packageClause,
      importsClause
    ).collect {
      case Some(str) => str
    }

    if (preRendered.isEmpty) None
    else Some(preRendered.mkString(doubleLineSeparator))
  }

  extension (strs: Seq[String]) {
    def mkStringIfNonEmpty(prefix: String, separator: String, suffix: String): String = {
      if(strs.isEmpty) {
        ""
      } else {
        strs.mkString(prefix, separator, suffix)
      }
    }
  }
}

object RenderUtils extends Render


