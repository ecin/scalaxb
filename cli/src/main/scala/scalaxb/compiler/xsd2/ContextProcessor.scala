package scalaxb.compiler.xsd2

import scalaxb.compiler.{ScalaNames, Logger, Config, ReferenceNotFound}
import xmlschema._
import Defs._
import scalaxb._

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

  def containsEnumeration(decl: XSimpleType)(implicit tag: HostTag): Boolean =
    !filterEnumeration(decl).isEmpty

  def filterEnumeration(tagged: Tagged[XSimpleType]): Seq[Tagged[XNoFixedFacet]] =
    filterEnumeration(tagged.value)(tagged.tag)

  def filterEnumeration(decl: XSimpleType)(implicit tag: HostTag): Seq[Tagged[XNoFixedFacet]] =
    decl.arg1.value match {
      case restriction: XRestriction =>
        restriction.arg1.arg2 collect {
          case DataRecord(_, Some("enumeration"), enum: XNoFixedFacet) => Tagged(enum, tag)
        }
      case _ => Nil
    }

  def nameTypes(elem: XElement)(implicit tag: HostTag) {
    elem.xelementoption map {_.value match {
      case x: XLocalComplexType =>
        names(Tagged(x, tag)) = makeProtectedTypeName(elem)
      case x: XLocalSimpleType if (containsEnumeration(x)) =>
        names(Tagged(x, tag)) = makeProtectedTypeName(elem)
        nameEnumValues(x)
      case _ =>
    }}
  }

  def nameTypes(decl: XSimpleType)(implicit tag: HostTag) {
    if (containsEnumeration(decl)) {
      names(Tagged(decl, tag)) = makeProtectedTypeName(decl)
      nameEnumValues(decl)
    }
  }

  def nameTypes(decl: XComplexType)(implicit tag: HostTag) {
    names(Tagged(decl, tag)) = makeProtectedTypeName(decl)
  }

  def nameEnumValues(decl: XSimpleType)(implicit tag: HostTag) {
    filterEnumeration(decl) map { enum =>
      names(enum) = makeProtectedTypeName(enum.value)
    }
  }

  def makeProtectedTypeName(elem: XElement)(implicit tag: HostTag): String =
    makeProtectedTypeName(elem.name, "")

  def makeProtectedTypeName(decl: XComplexType)(implicit tag: HostTag): String =
    makeProtectedTypeName(decl.name, "Type")

  def makeProtectedTypeName(decl: XSimpleType)(implicit tag: HostTag): String =
    makeProtectedTypeName(decl.name, "Type")

  def makeProtectedTypeName(enum: XNoFixedFacet)(implicit tag: HostTag): String =
    makeProtectedTypeName(enum.value, "Value")

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
