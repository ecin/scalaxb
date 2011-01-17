package scalaxb.compiler.xsd2

import xmlschema._
import scalaxb.compiler.xsd.{XsAny, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt}
import Defs._
import java.net.URI

trait Params extends Lookup {
  case class Occurrence(minOccurs: Int, maxOccurs: Int, nillable: Boolean)
  object Occurrence {
    def apply(minOccurs: Int, maxOccurs: String, nillable: Boolean): Occurrence =
      Occurrence(minOccurs,
        if (maxOccurs == "unbounded") Int.MaxValue
        else maxOccurs.toInt,
        nillable)

    def apply(tagged: Tagged[XElement]): Occurrence =
      Occurrence(tagged.value.minOccurs, tagged.value.maxOccurs, tagged.value.nillable)
  }

  val SingleNotNillable = Occurrence(1, 1, false)
  val SingleNillable = Occurrence(1, 1, true)
  val OptionalNotNillable = Occurrence(0, 1, false)
  val OptionalNillable = Occurrence(0, 1, true)
  val UnboundedNotNillable = Occurrence(0, Int.MaxValue, false)
  val UnboundedNillable = Occurrence(0, Int.MaxValue, true)

  case class Param(namespace: Option[URI],
    name: String,
    typeSymbol: Tagged[Any],
    occurrence: Occurrence,
    attribute: Boolean) {

    def baseTypeName: QualifiedName = buildTypeName(typeSymbol)

    def typeName: String = occurrence match {
      case SingleNotNillable   => baseTypeName.toScalaCode
      case SingleNillable      => "Option[%s]".format(baseTypeName.toScalaCode)
      case OptionalNotNillable => "Option[%s]".format(baseTypeName.toScalaCode)
      case OptionalNillable    => "Option[Option[%s]]".format(baseTypeName.toScalaCode)
      case _ =>
        if (!occurrence.nillable) "Seq[%s]".format(baseTypeName.toScalaCode)
        else "Seq[Option[%s]]".format(baseTypeName.toScalaCode)
    }

    def toTraitScalaCode: String =
      makeParamName(name) + ": " + typeName

    def toScalaCode: String =
      toTraitScalaCode + (
        if (occurrence == OptionalNotNillable && attribute) " = None"
        else "")
  }

  // tagged can be Tagged[XSimpleType], Tagged[BuiltInSymbol], Tagged[XElement], Tagged[KeyedGroup],
  // Tagged[XAny].
  def buildParam(tagged: Tagged[Any]) = tagged.value match {
    case decl: XSimpleType               => Param(tagged.tag.namespace, tagged.tag.name, tagged, SingleNotNillable, false)
    case symbol: BuiltInSimpleTypeSymbol => Param(tagged.tag.namespace, tagged.tag.name, tagged, SingleNotNillable, false)
    case elem: XElement                  => buildElementParam(Tagged(elem, tagged.tag))
    case group: KeyedGroup               => Param(tagged.tag.namespace, tagged.tag.name, tagged, SingleNotNillable, false)
    case any: XAny                       => Param(tagged.tag.namespace, "any", tagged, SingleNotNillable, false)
    case _ => error("buildParam: " + tagged)
  }

  def buildElementParam(tagged: Tagged[XElement]): Param = {
    val elem = tagged.value
    val name = elem.name.getOrElse(elem.ref map {_.toString} getOrElse {"??"})
    val occurrence = Occurrence(tagged)
    val retval = Param(tagged.tag.namespace, name, elementType(tagged), occurrence, false)
    log("Params#buildElementParam:  " + retval.toString)
    retval
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
