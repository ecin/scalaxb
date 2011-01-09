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

import scalaxb._
import xmlschema._
import scala.collection.immutable

object Defs {
  implicit def schemaToSchemaOps(schema: XSchema): SchemaOps = new SchemaOps(schema)
}

abstract class TopLevelType
case object SimpleTypeHost extends  TopLevelType
case object ComplexTypeHost extends TopLevelType
case object NamedGroupHost extends TopLevelType
case object AttributeGroupHost extends TopLevelType
case object ElementHost extends TopLevelType
case object AttributeHost extends TopLevelType
case class HostTag(namespace: Option[String], topLevel: TopLevelType, name: String)
object HostTag {
  def apply(namespace: Option[String], elem: XTopLevelElement): HostTag =
      HostTag(namespace, ElementHost, elem.name getOrElse {error("name is required.")})
  def apply(namespace: Option[String], decl: XTopLevelSimpleType): HostTag =
    HostTag(namespace, SimpleTypeHost, decl.name getOrElse {error("name is required.")})
  def apply(namespace: Option[String], decl: XTopLevelComplexType): HostTag =
      HostTag(namespace, ComplexTypeHost, decl.name getOrElse {error("name is required.")})
  def apply(namespace: Option[String], attr: XTopLevelAttribute): HostTag =
      HostTag(namespace, AttributeHost, attr.name getOrElse {error("name is required.")})
  def apply(namespace: Option[String], group: XNamedGroup): HostTag =
      HostTag(namespace, NamedGroupHost, group.name getOrElse {error("name is required.")})
  def apply(namespace: Option[String], group: XNamedAttributeGroup): HostTag =
      HostTag(namespace, AttributeGroupHost, group.name getOrElse {error("name is required.")})
}

case class KeyedGroup(key: String, group: XGroup)
class Tagged[+A](val value: A, val tag: HostTag) {
}

object Tagged {
  def apply[A](value: A, tag: HostTag) = new Tagged(value, tag)
  def unapply[A](value: Tagged[A]): Option[(A, HostTag)] = Some((value.value, value.tag))
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
  def toThat(decl: XSimpleType, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(decl, tag))
  def toThat(decl: XComplexType, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(decl, tag))
  def toThat(group: KeyedGroup, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(group, tag))
  def toThat(group: XAttributeGroup, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(group, tag))
  def toThat(elem: XElement, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(elem, tag))
  def toThat(attr: XAttributable, tag: HostTag): Option[Tagged[Any]] = Some(Tagged(attr, tag))

  def schemaToList(schema: XSchema): List[Tagged[Any]] = {
    val ns = schema.targetNamespace map { _.toString }

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
          processComplexType(x)(HostTag(ns, x))
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

  def processComplexType(decl: XComplexType)(implicit tag: HostTag): List[Tagged[Any]] = {
    // <xs:group ref="xs:typeDefParticle"/>
    // <xs:group ref="xs:simpleRestrictionModel"/>
    def processRestriction(restriction: XRestrictionTypable) =
      (restriction.xrestrictiontypableoption map { _ match {
        case DataRecord(_, _, XSimpleRestrictionModelSequence(Some(simpleType), _)) =>
          processSimpleType(simpleType)
        case DataRecord(_, Some(key), x: XTypeDefParticleOption) => processTypeDefParticle(key, x)
        case _ => Nil
      }} getOrElse {Nil}) ::: processAttrSeq(restriction.arg2)

    def processExtension(extension: XExtensionTypable) =
      (extension.arg1 map {
        case DataRecord(_, Some(key), x: XTypeDefParticleOption) => processTypeDefParticle(key, x)
        case _ => Nil
      } getOrElse {Nil}) ::: processAttrSeq(extension.arg2)

    toThat(decl, tag).toList :::
    (decl.arg1.value match {
      case XComplexContent(_, DataRecord(_, _, x: XComplexRestrictionType), _, _, _) => processRestriction(x)
      case XComplexContent(_, DataRecord(_, _, x: XExtensionType), _, _, _)          => processExtension(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleRestrictionType), _, _)      => processRestriction(x)
      case XSimpleContent(_, DataRecord(_, _, x: XSimpleExtensionType), _, _)        => processExtension(x)
      case XComplexTypeModelSequence1(arg1, arg2) =>
        (arg1 map {
          case DataRecord(_, Some(key), x: XTypeDefParticleOption) => processTypeDefParticle(key, x)
          case _ => Nil
        } getOrElse {Nil}) ::: processAttrSeq(arg2)
    })
  }

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
      case x: XLocalComplexType => processComplexType(x)
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

  // <xs:element name="group" type="xs:groupRef"/>
  // <xs:element ref="xs:all"/>
  // <xs:element ref="xs:choice"/>
  // <xs:element ref="xs:sequence"/>
  def processTypeDefParticle(key: String, particle: XTypeDefParticleOption)
                            (implicit tag: HostTag): List[Tagged[Any]] =
    particle match {
      case x: XGroupRef          => processGroup(KeyedGroup(key, x))
      case x: XExplicitGroupable => processGroup(KeyedGroup(key, x))
    }
}

