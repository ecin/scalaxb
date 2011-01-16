package scalaxb.compiler.xsd2

import xmlschema._
import scalaxb.compiler.xsd.{XsAny, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt}
import Defs._
import java.net.URI

trait Params extends Lookup {

  abstract class Cardinality
  case object Optional extends Cardinality { override def toString: String = "Optional" }
  case object Single extends Cardinality { override def toString: String = "Single" }
  case object Multiple extends Cardinality { override def toString: String = "Multiple" }

  def toCardinality(minOccurs: Int, maxOccurs: Int): Cardinality =
    if (maxOccurs > 1) Multiple
    else if (minOccurs == 0) Optional
    else Single

  case class Param(namespace: Option[URI],
    name: String,
    typeSymbol: Tagged[Any],
    cardinality: Cardinality,
    nillable: Boolean,
    attribute: Boolean) {

    def baseTypeName: String = buildTypeName(typeSymbol)

    def singleTypeName: String =
      if (nillable) "Option[" + baseTypeName + "]"
      else baseTypeName

    def typeName: String = cardinality match {
      case Single   => singleTypeName
      case Optional => "Option[" + singleTypeName + "]"
      case Multiple => "Seq[" + singleTypeName + "]"
    }

    def toTraitScalaCode: String =
      makeParamName(name) + ": " + typeName

    def toScalaCode: String =
      toTraitScalaCode + (
        if (cardinality == Optional && attribute) " = None"
        else "")
  }

  // tagged can be Tagged[XSimpleType], Tagged[BuiltInSymbol], Tagged[XElement], Tagged[KeyedGroup],
  // Tagged[XAny].
  def buildParam(tagged: Tagged[Any]) = tagged.value match {
    case decl: XSimpleType               => Param(tagged.tag.namespace, tagged.tag.name, tagged, Single,
                                              false, false)
    case symbol: BuiltInSimpleTypeSymbol => Param(tagged.tag.namespace, tagged.tag.name, tagged, Single, false, false)
    case elem: XElement                  => Param(tagged.tag.namespace, elem.name.getOrElse(elem.ref map {_.toString} getOrElse {""}),
      elementType(Tagged(elem, tagged.tag)), Single, false, false)
    case group: KeyedGroup               => Param(tagged.tag.namespace, tagged.tag.name, tagged, Single, false, false)
    case any: XAny                       => Param(tagged.tag.namespace, "any", tagged, Single, false, false)
    case _ => error("buildParam: " + tagged)
  }

  def elementType(tagged: Tagged[XElement]) = {
    val elem = tagged.value

    elem.name map { _ =>
      elem.typeValue map { typeValue =>
        resolveType(typeValue)
      } getOrElse {
        elem.xelementoption map { _.value match {
          case x: XLocalComplexType => Tagged(x, tagged.tag)
          case x: XLocalSimpleType  => Tagged(x, tagged.tag)
        }} getOrElse {error("type not found for element: " + tagged.value.toString)}
      }
    } getOrElse {
      Tagged(XsInt, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, "int")) }
  }
}
