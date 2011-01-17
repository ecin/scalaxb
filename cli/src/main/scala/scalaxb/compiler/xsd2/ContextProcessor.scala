package scalaxb.compiler.xsd2

import scalaxb.compiler.{ScalaNames, Logger, Config, ReferenceNotFound}
import xmlschema._
import Defs._

trait ContextProcessor extends ScalaNames {
  def logger: Logger
  def log(msg: String) = logger.log(msg)
  def config: Config
  def context: SchemaContext
  lazy val names = context.names

  def processSchema(schema: ReferenceSchema) {
    schema.unbound foreach { tagged =>
      implicit val tag = tagged.tag
      tagged.value match {
        case x: XTopLevelElement     => nameTypes(x)
        case x: XTopLevelSimpleType  => nameTypes(x)
        case x: XTopLevelComplexType => nameTypes(x)
        case _ =>
      }
    }

    schema.unbound foreach { tagged =>
      implicit val tag = tagged.tag
      tagged.value match {
        case x: XLocalElement     => nameTypes(x)
        // case x: XTopLevelSimpleType  => nameTypes(x)
        // case x: XLocalComplexType => nameTypes(x)
        case _ =>
      }
    }

  }

  def nameTypes(elem: XElement)(implicit tag: HostTag) {
    elem.xelementoption map {_.value match {
      case x: XLocalComplexType =>
        names(Tagged(x, tag)) = makeProtectedTypeName(elem)
      case x: XLocalSimpleType  =>
    }}
  }

  def nameTypes(decl: XSimpleType)(implicit tag: HostTag) {


  }

  def nameTypes(decl: XComplexType)(implicit tag: HostTag) {
    names(Tagged(decl, tag)) = makeProtectedTypeName(decl)
  }

  def makeProtectedTypeName(elem: XElement)(implicit tag: HostTag): String =
    makeProtectedTypeName(elem.name, "")

  def makeProtectedTypeName(decl: XComplexType)(implicit tag: HostTag): String =
    makeProtectedTypeName(decl.name, "Type")

  def makeProtectedTypeName(initialName: Option[String], postfix: String)(implicit tag: HostTag): String =
    makeProtectedTypeName(initialName getOrElse {error("name is required.")}, postfix)

  def makeProtectedTypeName(initialName: String, postfix: String)(implicit tag: HostTag): String = {
    makeTypeName(initialName)
  }

  def makeParamName(name: String) = {
    val base = config.paramPrefix map { p =>
      if (p.endsWith("_"))  p + name
      else p + name.capitalize
    } getOrElse { name }

    if (isKeyword(base)) identifier(base + "Value")
    else if (startsWithNumber(base)) identifier("number" + base)
    else identifier(base)
  }

  private def makeTypeName(name: String) = name match {
    case s if (s.startsWith("java.") || s.startsWith("javax.")) => s
    case _ =>
      val base = config.classPrefix map { p =>
        if (p.endsWith("_"))  p.capitalize + name
        else p.capitalize + name.capitalize
      } getOrElse { identifier(name).capitalize }
      if (startsWithNumber(base)) "Number" + base
      else if (isCommonlyUsedWord(base)) base + "Type"
      else base
  }

  private def startsWithNumber(name: String) =
    """\d""".r.findPrefixMatchOf(name) match {
      case Some(_) => true
      case _ => false
    }

  private def identifier(value: String) =
    """\W""".r.replaceAllIn(value, "")

  def quote(value: Option[String]): String = value map {
    "Some(\"" + _ + "\")"
  } getOrElse { "None" }

  def quote(value: String): String = if (value == null) "null"
    else "\"" + value + "\""

  def indent(indent: Int) = "  " * indent
}
