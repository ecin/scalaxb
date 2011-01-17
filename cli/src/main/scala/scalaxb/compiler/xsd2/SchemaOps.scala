/*
 * Copyright (c) 2010 e.e d3si9n
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package scalaxb.compiler.xsd2

import java.net.{URI}
import scala.xml.{NamespaceBinding}
import scalaxb._
import xmlschema._
import scala.collection.immutable

object Defs {
  implicit def schemaToSchemaOps(schema: XSchema): SchemaOps = new SchemaOps(schema)
  implicit def complexTypeToComplexTypeOps(decl: Tagged[XComplexType]): ComplexTypeOps = new ComplexTypeOps(decl)
  val XML_SCHEMA_URI = new URI("http://www.w3.org/2001/XMLSchema")
  val XSI_URL = new URI("http://www.w3.org/2001/XMLSchema-instance")
  val XSI_PREFIX = "xsi"
  val XML_URI = new URI("http://www.w3.org/XML/1998/namespace")
  val XML_PREFIX = "xml"
  val SCALAXB_URI = new URI("http://scalaxb.org/")
  val NL = System.getProperty("line.separator")

  val XS_ANY_TYPE = QualifiedName(XML_SCHEMA_URI, "anyType")
}

abstract class TopLevelType
case object SimpleTypeHost extends  TopLevelType
case object ComplexTypeHost extends TopLevelType
case object NamedGroupHost extends TopLevelType
case object AttributeGroupHost extends TopLevelType
case object ElementHost extends TopLevelType
case object AttributeHost extends TopLevelType
case class HostTag(namespace: Option[URI], topLevel: TopLevelType, name: String)
object HostTag {
  def apply(namespace: Option[URI], elem: XTopLevelElement): HostTag =
      HostTag(namespace, ElementHost, elem.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], decl: XTopLevelSimpleType): HostTag =
    HostTag(namespace, SimpleTypeHost, decl.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], decl: XTopLevelComplexType): HostTag =
      HostTag(namespace, ComplexTypeHost, decl.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], attr: XTopLevelAttribute): HostTag =
      HostTag(namespace, AttributeHost, attr.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], group: XNamedGroup): HostTag =
      HostTag(namespace, NamedGroupHost, group.name getOrElse {error("name is required.")})
  def apply(namespace: Option[URI], group: XNamedAttributeGroup): HostTag =
      HostTag(namespace, AttributeGroupHost, group.name getOrElse {error("name is required.")})
}

case class KeyedGroup(key: String, group: XGroup)
case class Tagged[+A](val value: A, val tag: HostTag) {
  override def toString: String = "Tagged(%s, %s)".format(value.toString, tag.toString)
}
object Tagged {
  implicit def box[A](value: A)(implicit tag: HostTag): Tagged[A] = Tagged[A](value, tag)
  implicit def unbox[A](tagged: Tagged[A]): A = tagged.value
}


class SchemaOps(val schema: XSchema) extends immutable.LinearSeq[Tagged[Any]] {
  lazy val length: Int = list.length
  def apply(index: Int): Tagged[Any] = list(index)

  override def isEmpty = list.isEmpty
  override def head = list.head
  override def tail= list.tail
  override def toList = list

  private lazy val list: List[Tagged[Any]] = SchemaOps.schemaToList(schema)
}

object SchemaOps {
  import Defs._

  def toThat(decl: XSimpleType, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(decl, tag))
  def toThat(decl: XComplexType, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(decl, tag))
  def toThat(group: KeyedGroup, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(group, tag))
  def toThat(group: XAttributeGroup, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(group, tag))
  def toThat(elem: XElement, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(elem, tag))
  def toThat(attr: XAttributable, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(attr, tag))

  def schemaToList(schema: XSchema): List[Tagged[Any]] = {
    val ns = schema.targetNamespace

    // <xs:element ref="xs:simpleType"/>
    // <xs:element ref="xs:complexType"/>
    // <xs:element ref="xs:group"/>
    // <xs:element ref="xs:attributeGroup"/>
    // <xs:element ref="xs:element"/>
    // <xs:element ref="xs:attribute"/>
    // <xs:element ref="xs:notation"/>
    schema.xschemasequence1.toList flatMap {
      case XSchemaSequence1(data, _) => data match {
        case DataRecord(_, _, x: XTopLevelSimpleType)  =>
          processSimpleType(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XTopLevelComplexType) =>
          Tagged(x, HostTag(ns, x)).toList
        case DataRecord(_, Some(key), x: XNamedGroup)  =>
          processGroup(KeyedGroup(key, x))(HostTag(ns, x))
        case DataRecord(_, _, x: XNamedAttributeGroup) =>
          processAttributeGroup(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XTopLevelElement)     =>
          processElement(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XTopLevelAttribute)   =>
          processAttribute(x)(HostTag(ns, x))
        case DataRecord(_, _, x: XNotation)            => Nil
      }
    }
  }

  def processSimpleType(decl: XSimpleType)(implicit tag: HostTag): List[Tagged[Any]] =
    toThat(decl, tag).toList :::
    (decl.arg1 match {
      case DataRecord(_, _, restriction: XRestriction) =>
        restriction.arg1.simpleType map { processSimpleType } getOrElse {Nil}
      case DataRecord(_, _, list: XList) =>
        list.simpleType map { processSimpleType } getOrElse {Nil}
      case DataRecord(_, _, x: XUnion) => Nil
    })

  def processGroup(group: KeyedGroup)(implicit tag: HostTag): List[Tagged[Any]] = {
    // all, choice, and sequence are XExplicitGroupable, which are XGroup.
    // <xs:element name="element" type="xs:localElement"/>
    // <xs:element name="group" type="xs:groupRef"/>
    // <xs:element ref="xs:all"/>
    // <xs:element ref="xs:choice"/>
    // <xs:element ref="xs:sequence"/>
    // <xs:element ref="xs:any"/>
    def processParticle(particleKey: String, particle: XParticleOption) =
      particle match {
        case x: XLocalElementable  => processElement(x)
        case x: XGroupRef          => processGroup(KeyedGroup(particleKey, x))
        case x: XExplicitGroupable => processGroup(KeyedGroup(particleKey, x))
        case x: XAny               => Nil
      }

    toThat(group, tag).toList :::
    (group.group.arg1.toList.flatMap {
      case DataRecord(_, Some(particleKey), x: XParticleOption) => processParticle(particleKey, x)
      case _ => Nil
    })
  }

  def processAttributeGroup(group: XAttributeGroup)(implicit tag: HostTag): List[Tagged[Any]] =
    toThat(group, tag).toList :::
    processAttrSeq(group.arg1)

  def processElement(elem: XElement)(implicit tag: HostTag): List[Tagged[Any]] =
    toThat(elem, tag).toList :::
    (elem.xelementoption map { _.value match {
      case x: XLocalComplexType => Tagged(x, tag).toList
      case x: XLocalSimpleType  => processSimpleType(x)
    }} getOrElse {Nil})

  def processAttribute(attr: XAttributable)(implicit tag: HostTag): List[Tagged[Any]] =
    toThat(attr, tag).toList :::
    (attr.simpleType map { processSimpleType } getOrElse {Nil})

  def processAttrSeq(attrSeq: XAttrDeclsSequence)(implicit tag: HostTag): List[Tagged[Any]] =
    attrSeq.xattrdeclsoption1.toList flatMap {
      case DataRecord(_, _, x: XAttributable)      => processAttribute(x)
      case DataRecord(_, _, x: XAttributeGroupRef) => processAttributeGroup(x)
    }
}

class ComplexTypeOps(val decl: Tagged[XComplexType]) extends immutable.LinearSeq[Tagged[Any]] {
  lazy val length: Int = list.length
  def apply(index: Int): Tagged[Any] = list(index)

  override def isEmpty = list.isEmpty
  override def head = list.head
  override def tail= list.tail
  override def toList = list

  private lazy val list: List[Tagged[Any]] = ComplexTypeOps.complexTypeToList(decl)
  def particles(implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding) =
    ComplexTypeOps.complexTypeToParticles(decl)
}

object ComplexTypeOps {
  def complexTypeToList(decl: Tagged[XComplexType]): List[Tagged[Any]] = {
    implicit val tag = decl.tag

    // <xs:group ref="xs:typeDefParticle"/>
    // <xs:group ref="xs:simpleRestrictionModel"/>
    def processRestriction(restriction: XRestrictionTypable) =
      (restriction.xrestrictiontypableoption map { _ match {
        case DataRecord(_, _, XSimpleRestrictionModelSequence(Some(simpleType), _)) =>
          SchemaOps.processSimpleType(simpleType)
        // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
        case DataRecord(_, Some(key), x: XGroupRef)          => SchemaOps.processGroup(KeyedGroup(key, x))
        case DataRecord(_, Some(key), x: XExplicitGroupable) => SchemaOps.processGroup(KeyedGroup(key, x))
        case _ => Nil
      }} getOrElse {Nil}) ::: SchemaOps.processAttrSeq(restriction.arg2)

    def processExtension(extension: XExtensionTypable) =
      (extension.arg1 map {
        // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
        case DataRecord(_, Some(key), x: XGroupRef)          => SchemaOps.processGroup(KeyedGroup(key, x))
        case DataRecord(_, Some(key), x: XExplicitGroupable) => SchemaOps.processGroup(KeyedGroup(key, x))
        case _ => Nil
      } getOrElse {Nil}) ::: SchemaOps.processAttrSeq(extension.arg2)

    List(decl) :::
    (decl.value.arg1.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) => processRestriction(x)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          => processExtension(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => processRestriction(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => processExtension(x)

      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2) =>
        (arg1 map {
          // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
          case DataRecord(_, Some(key), x: XGroupRef)          => SchemaOps.processGroup(KeyedGroup(key, x))
          case DataRecord(_, Some(key), x: XExplicitGroupable) => SchemaOps.processGroup(KeyedGroup(key, x))
          case _ => Nil
        } getOrElse {Nil}) ::: SchemaOps.processAttrSeq(arg2)
    })
  }

  def innerSequenceToParticles(tagged: Tagged[KeyedGroup])
    (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): List[Tagged[Any]] = {
    val seq = tagged.value.group
    if (seq.minOccurs != 1 || seq.maxOccurs != "1")
      if (seq.arg1.length == 1) seq.arg1(0) match {
        case DataRecord(_, Some(particleKey), any: XAny) =>
          List(Tagged(any.copy(
            minOccurs = math.min(any.minOccurs, seq.minOccurs),
            maxOccurs = lookup.max(any.maxOccurs, seq.maxOccurs)), tagged.tag))
        case DataRecord(_, Some("choice"), choice: XExplicitGroup) =>
          List(Tagged(KeyedGroup("choice", choice.copy(
            minOccurs = math.min(choice.minOccurs, seq.minOccurs),
            maxOccurs = lookup.max(choice.maxOccurs, seq.maxOccurs)) ), tagged.tag))

        case _ => List(tagged)
      }
      else List(tagged)
    else lookup.splitLongSequence(tagged)
  }

  /** particles of the given decl flattened one level.
   * returns list of Tagged[XSimpleType], Tagged[BuiltInSimpleTypeSymbol], Tagged[XElement], Tagged[KeyedGroup],
   * Tagged[XAny].
   */
  def complexTypeToParticles(decl: Tagged[XComplexType])
    (implicit lookup: Lookup, targetNamespace: Option[URI], scope: NamespaceBinding): List[Tagged[Any]] = {
    import lookup._
    implicit val tag = decl.tag

    def processInnerParticle(particleKey: String, particle: XParticleOption) =
      particle match {
        case x: XLocalElementable  => List(Tagged(x, tag))
        case x: XGroupRef          => List(Tagged(KeyedGroup(particleKey, x), tag))
        case x: XExplicitGroupable =>
          if (particleKey == "sequence") innerSequenceToParticles(Tagged(KeyedGroup(particleKey, x), tag))
          else List(Tagged(KeyedGroup(particleKey, x), tag))
        case x: XAny               => List(Tagged(x, tag))
      }

    def toParticles(group: KeyedGroup): List[Tagged[Any]] =
      if (group.key == "sequence") (group.group.arg1.toList.flatMap {
        case DataRecord(_, Some(particleKey), x: XParticleOption) => processInnerParticle(particleKey, x)
        case _ => Nil
      })
      else List(Tagged(group, tag))

    def processRestriction(restriction: XRestrictionTypable) = {
      val base: QualifiedName = restriction.base
      base match {
        case BuiltInType(tagged) => List(tagged)
        case SimpleType(tagged)  => List(tagged)

        // if base is a complex type, keep the same for inheritance, otherwise it should be anyType
        case ComplexType(tagged) => complexTypeToParticles(tagged)

        // restriction of anyType
        case _ => restriction.xrestrictiontypableoption map { _ match {
          // see http://www.w3.org/TR/xmlschema-1/#Complex_Type_Definitions for details.
          case DataRecord(_, _, x@XSimpleRestrictionModelSequence(_, _)) =>
            x.simpleType map { simpleType => List(Tagged(simpleType, tag)) } getOrElse {Nil}

          // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
          case DataRecord(_, Some(key), x: XGroupRef)          => toParticles(KeyedGroup(key, x))
          case DataRecord(_, Some(key), x: XExplicitGroupable) => toParticles(KeyedGroup(key, x))
          case _ => Nil
        }} getOrElse {Nil}
      } // base match
    } // processRestriction

    def processExtension(extension: XExtensionTypable) =  {
      val base: QualifiedName = extension.base
      base match {
        case BuiltInType(tagged) => List(tagged)
        case SimpleType(tagged)  => List(tagged)
        case ComplexType(tagged) =>
          extension.arg1 map {
            // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
            case DataRecord(_, Some(key), x: XGroupRef)          =>
              complexTypeToParticles(tagged) ::: toParticles(KeyedGroup(key, x))
            case DataRecord(_, Some(key), x: XExplicitGroupable) =>
              complexTypeToParticles(tagged) ::: toParticles(KeyedGroup(key, x))
            case _ => complexTypeToParticles(tagged)
          } getOrElse { complexTypeToParticles(tagged) }

        // extension of anyType.
        case _ =>
          extension.arg1 map {
            // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
            case DataRecord(_, Some(key), x: XGroupRef)          => toParticles(KeyedGroup(key, x))
            case DataRecord(_, Some(key), x: XExplicitGroupable) => toParticles(KeyedGroup(key, x))
            case _ => Nil
          } getOrElse {Nil}
      } // base match
    } // processExtension

    decl.value.arg1.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) => processRestriction(x)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          => processExtension(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => processRestriction(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => processExtension(x)

      // this is an abbreviated form of xs:anyType restriction.
      case XComplexTypeModelSequence1(arg1, arg2) =>
        arg1 map {
          // XTypeDefParticleOption is either XGroupRef or XExplicitGroupable
          case DataRecord(_, Some(key), x: XGroupRef)          => toParticles(KeyedGroup(key, x))
          case DataRecord(_, Some(key), x: XExplicitGroupable) => toParticles(KeyedGroup(key, x))
          case _ => Nil
        } getOrElse {Nil}
    }

  }
}
