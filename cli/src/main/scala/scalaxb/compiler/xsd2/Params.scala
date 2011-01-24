package scalaxb.compiler.xsd2

import scalaxb._
import xmlschema._
import scalaxb.compiler.xsd.{XsAnyType, BuiltInSimpleTypeSymbol, XsTypeSymbol, XsInt}
import Defs._
import java.net.URI

trait Params extends Lookup {
  case class Param(namespace: Option[URI],
    name: String,
    typeSymbol: Tagged[Any],
    occurrence: Occurrence,
    attribute: Boolean) {

    def baseTypeName: QualifiedName = buildTypeName(typeSymbol)

    def typeName(implicit targetNamespace: Option[URI]): String = occurrence match {
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

    def toScalaCode(implicit targetNamespace: Option[URI]): String =
      toTraitScalaCode + (
        if (occurrence == OptionalNotNillable && attribute) " = None"
        else "")
  }

  object Param {
    def fromList(particles: List[Tagged[Any]]): List[Param] = {
      var anyNumber: Int = 0
      particles map { tagged => tagged.value match {
        case any: XAny =>
          anyNumber += 1
          buildParam(tagged, anyNumber)
        case _         => buildParam(tagged, 0)
      }}
    }

    // tagged can be Tagged[XSimpleType], Tagged[BuiltInSymbol], Tagged[XElement], Tagged[KeyedGroup],
    // Tagged[XAny].
    private def buildParam(tagged: Tagged[Any], postfix: Int) = tagged match {
      case TaggedSimpleType(decl, tag) => Param(tagged.tag.namespace, tagged.tag.name, tagged, SingleNotNillable, false)
      case TaggedSymbol(symbol, tag)   => Param(tagged.tag.namespace, tagged.tag.name, tagged, SingleNotNillable, false)
      case x: TaggedElement            => buildElementParam(x)
      case x: TaggedKeyedGroup if x.key == ChoiceTag => buildChoiceParam(x)
      case x: TaggedKeyedGroup         => buildCompositorParam(x)
      case x: TaggedAny                => buildAnyParam(x, postfix)
      case _ => error("buildParam: " + tagged)
    }

    private def buildElementParam(tagged: Tagged[XElement]): Param = {
      val elem = tagged.value
      val name = elem.name.getOrElse(elem.ref map {_.toString} getOrElse {"??"})
      val typesymbol = elem.name map { _ =>
        elem.typeValue map { typeValue =>
          resolveType(typeValue)
        } getOrElse {
          elem.xelementoption map { _.value match {
            case x: XLocalComplexType => Tagged(x, tagged.tag)
            case x: XLocalSimpleType  => Tagged(x, tagged.tag)
          }} getOrElse {error("type not found for element: " + tagged.value.toString)}
        }
      } getOrElse { Tagged(XsInt, HostTag(Some(XML_SCHEMA_URI), SimpleTypeHost, "int")) }

      val retval = Param(tagged.tag.namespace, name, typesymbol, Occurrence(elem), false)
      log("Params#buildElementParam:  " + retval.toString)
      retval
    }

    private def buildChoiceParam(tagged: Tagged[KeyedGroup]): Param = {
      implicit val tag = tagged.tag
      val choice = tagged.value
      val name = names.get(tagged) map {_.toLowerCase} getOrElse {"??"}
      val particles = choice.particles

      val memberType = choice.particles match {
        case Nil       => TaggedXsAnyType
        case x :: xs =>
          val sameType = x match {
            case elem: TaggedElement =>
              val firstType = particleType(x)
              if (xs forall { particleType(_) == firstType }) firstType
              else None
            case _ => None
          }

          sameType getOrElse {
            if ( (particles forall { !isForeignType(_) }) &&
              (particles forall { isOptionDescendant }) ) tagged
            else TaggedXsAnyType
          }
      }
      val typeSymbol = TaggedDataRecordSymbol(DataRecordSymbol(memberType))
      Param(tagged.tag.namespace, name, typeSymbol,
        Occurrence(choice).copy(nillable = false), false)
    }

    private def particleType(particle: Tagged[_]) = particle match {
      case elem: TaggedElement => Some(elem.typeStructure)
      case _ => None
    }

    private def buildCompositorParam(tagged: Tagged[KeyedGroup]): Param = {
      val compositor = tagged.value
      val name = names.get(tagged) map {_.toLowerCase} getOrElse {"??"}
      val typeSymbol = tagged
      Param(tagged.tag.namespace, name, typeSymbol, Occurrence(compositor), false)
    }

    private def buildAnyParam(tagged: Tagged[XAny], postfix: Int): Param = {
      val any = tagged.value
      val name = if (postfix <= 1) "any"
        else "any" + postfix.toString
      val retval = Param(tagged.tag.namespace, name, tagged, Occurrence(any), false)
      log("Params#buildAnyParam:  " + retval.toString)
      retval
    }
  } // object Param
}
