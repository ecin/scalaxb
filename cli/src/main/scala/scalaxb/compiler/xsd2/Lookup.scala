package scalaxb.compiler.xsd2

import java.net.{URI}
import javax.xml.namespace.{QName}
import xmlschema._
import scalaxb.compiler.{ScalaNames, Logger, Config, Snippet, ReferenceNotFound}
import scalaxb.compiler.xsd.{XsAny, XsString, BuiltInSimpleTypeSymbol, XsTypeSymbol}
import Defs._
import scala.xml.{NamespaceBinding}

case class QualifiedName(namespace: Option[URI], localPart: String) {
  def toScalaCode(implicit targetNamespace: Option[URI], lookup: Lookup): String =
    if (namespace == targetNamespace || namespace.isEmpty) localPart
    else lookup.packageName(namespace) + "." + localPart
}

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

trait Lookup extends ContextProcessor {
  implicit val lookup = this

  def schema: ReferenceSchema
  implicit def scope: NamespaceBinding = schema.scope
  implicit def targetNamespace = schema.targetNamespace

  def buildTypeName(tagged: Tagged[Any]): QualifiedName = {
    implicit val tag = tagged.tag

    tagged.value match {
      case XsAny                           => QualifiedName(Some(SCALAXB_URI), "DataRecord[Any]")
      case any: XAny                       => QualifiedName(Some(SCALAXB_URI), "DataRecord[Any]")
      case symbol: BuiltInSimpleTypeSymbol => QualifiedName(None, symbol.name)
      case decl: XSimpleType               => buildSimpleTypeTypeName(decl)
      case decl: XComplexType              =>
        QualifiedName(tagged.tag.namespace, names.get(decl) getOrElse {"??"})
      case enum: XNoFixedFacet             =>
        QualifiedName(tagged.tag.namespace, names.get(enum) getOrElse {"??"})
      case keyedGroup: KeyedGroup          =>
        QualifiedName(tagged.tag.namespace, keyedGroup.key)
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
      case _ => error("buildTypeName # unsupported: " + tagged)
    }
  }

  def buildSimpleTypeTypeName(decl: XSimpleType)(implicit tag: HostTag): QualifiedName = {
    decl.arg1.value match {
      case restriction: XRestriction if containsEnumeration(decl) =>
        // trace type hierarchy to the top most type that implements enumeration.
        val base = baseType(decl)
        QualifiedName(base.tag.namespace, names.get(base) getOrElse {"??"})
      case restriction: XRestriction =>
        buildTypeName(baseType(decl))
      case list: XList =>
        val base = baseType(decl)
        val baseName = base.value match {
          case symbol: BuiltInSimpleTypeSymbol => symbol.name
          case decl: XSimpleType               => names.get(base) getOrElse {"??"}
        }
        QualifiedName(None, "Seq[%s]".format(QualifiedName(base.tag.namespace, baseName).toScalaCode))
      // union baseType is hardcoded to xs:string.
      case union: XUnion =>
        buildTypeName(baseType(decl))
    }
  }

  def baseType(decl: Tagged[XSimpleType]): Tagged[Any] = decl.value.arg1.value match {
    case XRestriction(_, _, _, Some(base), _) if containsEnumeration(decl) =>
      QualifiedName.fromQName(base) match {
        case BuiltInType(tagged) => decl
        case SimpleType(tagged)  =>
          if (containsEnumeration(tagged)) baseType(tagged)
          else decl
      }
    case XRestriction(_, _, _, Some(base), _) =>
      QualifiedName.fromQName(base) match {
        case BuiltInType(tagged) => tagged
        case SimpleType(tagged)  => baseType(tagged)
      }
    case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) if containsEnumeration(decl) =>
      if (containsEnumeration(Tagged(simpleType, decl.tag))) baseType(Tagged(simpleType, decl.tag))
      else decl
    case XRestriction(_, XSimpleRestrictionModelSequence(Some(simpleType), _), _, _, _) =>
      baseType(Tagged(simpleType, decl.tag))
    case XList(_, _, _, Some(itemType), _) =>
      QualifiedName.fromQName(itemType) match {
        case BuiltInType(tagged) => tagged
        case SimpleType(tagged)  => baseType(tagged)
      }
    case XList(_, Some(simpleType), _, _, _) =>
      baseType(Tagged(simpleType, decl.tag))
    case x: XUnion => Tagged(XsString, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, "string"))
    case _ => error("baseType#: Unsupported content " +  decl.arg1.value.toString)
  }

  def resolveType(typeName: QualifiedName): Tagged[Any] = typeName match {
    case AnyType(tagged)     => tagged
    case BuiltInType(tagged) => tagged
    case SimpleType(tagged)  => tagged
    case ComplexType(tagged) => tagged
    case _ => throw new ReferenceNotFound("type", typeName.namespace map {_.toString}, typeName.localPart)
  }

  object AnyType {
    def unapply(typeName: QualifiedName): Option[Tagged[XsTypeSymbol]] = typeName match {
      case XS_ANY_TYPE => Some(Tagged(XsAny, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, "anyType")))
      case _ => None
    }
  }

  object BuiltInType {
    def unapply(typeName: QualifiedName): Option[Tagged[XsTypeSymbol]] = typeName match {
      case QualifiedName(Some(XML_SCHEMA_URI), localPart) =>
        Some(Tagged(XsTypeSymbol.toTypeSymbol(localPart), HostTag(typeName.namespace, SimpleTypeHost, localPart)))
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

  def packageName(namespace: Option[URI]): String =
    namespace map { ns =>
      if (ns == SCALAXB_URI) "scalaxb"
      else "packagename" } getOrElse { "packagename" }
}
