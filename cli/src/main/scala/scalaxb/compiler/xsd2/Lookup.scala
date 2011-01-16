package scalaxb.compiler.xsd2

import java.net.{URI}
import javax.xml.namespace.{QName}
import xmlschema._
import scalaxb.compiler.{ScalaNames, Logger, Config, Snippet, ReferenceNotFound}
import Defs._
import scalaxb.compiler.xsd.{XsAny, XsInt, XsTypeSymbol, BuiltInSimpleTypeSymbol}
import scala.xml.{NamespaceBinding}

case class QualifiedName(namespace: Option[URI], localPart: String)

object QualifiedName {
  def apply(namespace: URI, name: String): QualifiedName = QualifiedName(Some(namespace), name)

  implicit def fromQName(value: QName)(implicit targetNamespace: Option[URI], scope: NamespaceBinding) =
    splitTypeName(value.toString)

  def splitTypeName(name: String)(implicit targetNamespace: Option[URI], scope: NamespaceBinding): QualifiedName =
    if (name.contains('@')) QualifiedName(targetNamespace, name)
    else if (name.contains(':')) {
        val prefix = name.dropRight(name.length - name.indexOf(':'))
        val value = name.drop(name.indexOf(':') + 1)
        QualifiedName(Option[String](scope.getURI(prefix)) map {new URI(_)}, value)
      }
      else QualifiedName(Option[String](scope.getURI(null)) map {new URI(_)}, name)
}

trait Lookup extends ScalaNames {
  implicit val lookup = this

  def config: Config
  def schema: ReferenceSchema
  implicit def scope: NamespaceBinding = schema.scope
  implicit def targetNamespace = schema.targetNamespace

  def typeName(decl: Tagged[XComplexType]) =
    FullName(None, decl.tag.name)

  def resolveType(typeName: QualifiedName): Tagged[Any] = typeName match {
    case AnyType(tagged)     => tagged
    case BuiltInType(tagged) => tagged
    case SimpleType(tagged)  => tagged
    case ComplexType(tagged) => tagged
    case _ => throw new ReferenceNotFound("type", typeName.namespace map {_.toString}, typeName.localPart)
  }

  object AnyType {
    def unapply(typeName: QualifiedName): Option[Tagged[XsTypeSymbol]] = typeName match {
      case XS_ANY_TYPE => Some(Tagged(XsAny, HostTag(Some(XML_URI), SimpleTypeHost, "anyType")))
      case _ => None
    }
  }

  object BuiltInType {
    def unapply(typeName: QualifiedName): Option[Tagged[XsTypeSymbol]] = typeName match {
      case QualifiedName(Some(XML_SCHEMA_URI), localPart) => Some(Tagged(XsTypeSymbol.toTypeSymbol(localPart),
        HostTag(typeName.namespace, SimpleTypeHost, localPart)))
      case _ => None
    }
  }

  object SimpleType {
    def unapply(typeName: QualifiedName): Option[Tagged[XSimpleType]] = typeName match {
      case QualifiedName(targetNamespace, localPart) if schema.topTypes contains localPart =>
        schema.topTypes(typeName.localPart) match {
          case Tagged(value, tag) => value match {
            case x: XSimpleType => Some(Tagged(x, tag))
            case _ => None
          }
        }
      case _ => None
    }
  }

  object ComplexType {
    def unapply(typeName: QualifiedName): Option[Tagged[XComplexType]] = typeName match {
      case QualifiedName(targetNamespace, localPart) if schema.topTypes contains localPart =>
        schema.topTypes(typeName.localPart) match {
          case Tagged(value, tag) => value match {
            case x: XComplexType => Some(Tagged(x, tag))
            case _ => None
          }
        }
      case _ => None
    }
  }

  def splitLongSequence(tagged: Tagged[KeyedGroup]): List[Tagged[KeyedGroup]] = List(tagged)

  def max(lhs: String, rhs: String): String =
    if (lhs == "unbounded" || rhs == "unbounded") "unbounded"
    else math.max(lhs.toInt, rhs.toInt).toString


  def buildTypeName(tagged: Tagged[Any], shortLocal: Boolean = false): String = tagged.value match {
    case XsAny                           => "scalaxb.DataRecord[Any]"
    case symbol: BuiltInSimpleTypeSymbol => symbol.name
    case decl: XSimpleType               => decl.name getOrElse {"??"}
    case decl: XComplexType              => decl.name getOrElse {"??"}

//
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
    case _ => "??"
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
