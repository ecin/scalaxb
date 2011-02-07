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
    schema.unbound foreach {
      case tagged: TaggedElement =>
        tagged.value match {
          case x: XTopLevelElement => nameElementTypes(tagged)
          case _ =>
        }
      case tagged: TaggedSimpleType =>
        tagged.value match {
          case x: XTopLevelSimpleType => nameSimpleTypes(tagged)
          case _ =>
        }
      case tagged: TaggedComplexType =>
        tagged.value match {
          case x: XTopLevelComplexType => nameComplexTypes(tagged)
          case _ =>
        }
      case _ =>
    }

    schema.unbound foreach {
      case tagged: TaggedElement =>
        tagged.value match {
          case x: XLocalElement => nameElementTypes(tagged)
          case _ =>
        }
      case _ =>
    }

  }

  def containsEnumeration(tagged: Tagged[Any]): Boolean = tagged match {
    case x: TaggedSimpleType => !filterEnumeration(x).isEmpty
    case _ => false
  }

  def containsEnumeration(decl: XSimpleType)(implicit tag: HostTag): Boolean =
    !filterEnumeration(decl).isEmpty

  def filterEnumeration(tagged: Tagged[XSimpleType]): Seq[Tagged[XNoFixedFacet]] =
    filterEnumeration(tagged.value)(tagged.tag)

  def filterEnumeration(decl: XSimpleType)(implicit tag: HostTag): Seq[Tagged[XNoFixedFacet]] =
    decl.arg1.value match {
      case restriction: XRestriction =>
        restriction.arg1.arg2 collect {
          case DataRecord(_, Some("enumeration"), enum: XNoFixedFacet) => TaggedEnum(enum, tag)
        }
      case _ => Nil
    }

  def nameElementTypes(elem: Tagged[XElement]) {
    implicit val tag = elem.tag

    elem.xelementoption map {_.value match {
      case x: XLocalComplexType =>
        names(Tagged(x, tag)) = makeProtectedElementTypeName(elem)
      case x: XLocalSimpleType if (containsEnumeration(x)) =>
        names(Tagged(x, tag)) = makeProtectedElementTypeName(elem)
        nameEnumValues(Tagged(x, elem.tag))
      case _ =>
    }}
  }

  def nameSimpleTypes(decl: Tagged[XSimpleType]) {
    if (containsEnumeration(decl)) {
      names(decl) = makeProtectedSimpleTypeName(decl)
      nameEnumValues(decl)
    }
  }

  def nameComplexTypes(decl: Tagged[XComplexType]) {
    names(decl) = makeProtectedComplexTypeName(decl)
    val primarySequence = decl.primarySequence
    decl collect {
      case Compositor(compositor) if Some(compositor) != primarySequence =>
        nameCompositor(compositor)
    }
  }

  def nameCompositor(tagged: Tagged[KeyedGroup]) {
    tagged.value.key match {
      case ChoiceTag   => names(tagged) = makeProtectedTypeName(tagged.tag.name + "Option", "", tagged.tag, false)
      case SequenceTag => names(tagged) = makeProtectedTypeName(tagged.tag.name + "Sequence", "", tagged.tag, false)
      case AllTag      => names(tagged) = makeProtectedTypeName(tagged.tag.name + "All", "", tagged.tag, false)
      case _ =>
    }
  }

  def nameEnumValues(decl: Tagged[XSimpleType]) {
    filterEnumeration(decl) map { enum =>
      names(enum) = makeProtectedEnumTypeName(enum)
    }
  }

  def makeProtectedElementTypeName(elem: Tagged[XElement]): String =
    makeProtectedTypeName(elem.name, "", elem.tag, true)

  def makeProtectedComplexTypeName(decl: Tagged[XComplexType]): String =
    makeProtectedTypeName(decl.name, "Type", decl.tag, true)

  def makeProtectedSimpleTypeName(decl: Tagged[XSimpleType]): String =
    makeProtectedTypeName(decl.name, "Type", decl.tag, true)

  def makeProtectedEnumTypeName(enum: Tagged[XNoFixedFacet]): String =
    makeProtectedTypeName(enum.value.value, "Value", enum.tag, true)

  def makeProtectedTypeName(initialName: Option[String], postfix: String, tag: HostTag, appendHost: Boolean): String =
    makeProtectedTypeName(initialName getOrElse {error("name is required.")}, postfix, tag, appendHost)

  def makeProtectedTypeName(initialName: String, postfix: String, tag: HostTag, appendHost: Boolean): String = {
    def contains(s: String) = names.valuesIterator.contains(s)

    var name = makeTypeName(initialName)
    if (!contains(name)) name
    else {
      name = makeTypeName(tag.name + initialName.capitalize)
      if (appendHost && !contains(name) && initialName != tag.name) name
      else {
        name = makeTypeName(initialName) + postfix
        for (i <- 2 to 100) {
          if (contains(name)) name = makeTypeName(initialName) + postfix + i
        } // for i
        name
      }
    }
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
