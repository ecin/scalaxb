package scalaxb.compiler.xsd2

import javax.xml.namespace.{QName}
import xmlschema._
import scalaxb.compiler.{ScalaNames, Logger, Config, Snippet}
import Defs._
import scalaxb.compiler.xsd.{XsAny, XsInt, XsTypeSymbol, BuiltInSimpleTypeSymbol}

trait Lookup extends ScalaNames {
  implicit val lookup = this

  def config: Config

  def typeName(decl: Tagged[XComplexType]) =
    FullName(None, decl.tag.name)

  def resolveType(typeName: QName): Tagged[Any] =
    if (typeName == XS_ANY_TYPE) Tagged(XsAny, HostTag(Some(XML_URI), SimpleTypeHost, "anyType"))
    else simpleType(typeName) getOrElse {
      complexType(typeName) getOrElse {
        error("type was not found: " + typeName.toString)
      }
    }

  def simpleType(typeName: QName): Option[Tagged[Any]] =
    if (XsTypeSymbol.toTypeSymbol.isDefinedAt(typeName.getLocalPart))
      Some(Tagged(XsTypeSymbol.toTypeSymbol(typeName.getLocalPart),
        HostTag(Some(typeName.getNamespaceURI), SimpleTypeHost, typeName.getLocalPart)))
    //if (typeName.getNamespaceURI == XML_SCHEMA_URI) Some(Tagged(XsTypeSymbol.toTypeSymbol(typeName.getLocalPart),
    //  HostTag(Some(typeName.getNamespaceURI), SimpleTypeHost, typeName.getLocalPart)))
    else Some(Tagged(XsInt, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, typeName.toString)))

  def complexType(typeName: QName): Option[Tagged[XComplexType]] = None

  def splitLongSequence(tagged: Tagged[KeyedGroup]): List[Tagged[KeyedGroup]] = List(tagged)

  def max(lhs: String, rhs: String): String =
    if (lhs == "unbounded" || rhs == "unbounded") "unbounded"
    else math.max(lhs.toInt, rhs.toInt).toString


  def buildTypeName(tagged: Tagged[Any], shortLocal: Boolean = false): String = tagged.value match {
    case symbol: BuiltInSimpleTypeSymbol => symbol.name
//    case XsAny          => "scalaxb.DataRecord[Any]"
//    case XsNillableAny  => "scalaxb.DataRecord[Option[Any]]"
//    case XsLongAll      => "Map[String, scalaxb.DataRecord[Any]]"
//    case XsLongAttribute => "Map[String, scalaxb.DataRecord[Any]]"
//    case XsAnyAttribute  => "Map[String, scalaxb.DataRecord[Any]]"
//    case XsDataRecord(ReferenceTypeSymbol(decl: ComplexTypeDecl)) if compositorWrapper.contains(decl) =>
//      compositorWrapper(decl) match {
//        case choice: ChoiceDecl => buildChoiceTypeName(decl, choice, shortLocal)
//        case _ => "scalaxb.DataRecord[Any]"
//      }
//    case r: XsDataRecord => "scalaxb.DataRecord[Any]"
//    case XsMixed         => "scalaxb.DataRecord[Any]"
//    case ReferenceTypeSymbol(decl: SimpleTypeDecl) => buildTypeName(decl, shortLocal)
//    case ReferenceTypeSymbol(decl: ComplexTypeDecl) => buildTypeName(decl, shortLocal)
//    case symbol: AttributeGroupSymbol => buildTypeName(attributeGroups(symbol.namespace, symbol.name), shortLocal)
//    case XsXMLFormat(decl: ComplexTypeDecl) => "scalaxb.XMLFormat[" + buildTypeName(decl, shortLocal) + "]"
//    case XsXMLFormat(group: AttributeGroupDecl) => "scalaxb.XMLFormat[" + buildTypeName(group, shortLocal) + "]"
    case _ => ""
  }

  def startsWithNumber(name: String) =
  """\d""".r.findPrefixMatchOf(name) match {
    case Some(_) => true
    case _ => false
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

//  def makePrefix(namespace: Option[String], context: SchemaContext): String = namespace map { ns =>
//    if (ns == XML_URI) XML_PREFIX
//    else context.prefixes.getOrElse(ns, "")
//  } getOrElse {""}

  def identifier(value: String) =
    """\W""".r.replaceAllIn(value, "")

  def quote(value: Option[String]): String = value map {
    "Some(\"" + _ + "\")"
  } getOrElse { "None" }

  def quote(value: String): String = if (value == null) "null"
    else "\"" + value + "\""

  def indent(indent: Int) = "  " * indent
}

case class FullName(packageName: Option[String], localName: String) {
}
