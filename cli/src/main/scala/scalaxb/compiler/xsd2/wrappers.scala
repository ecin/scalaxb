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

import scala.collection.immutable
import scalaxb._
import xmlschema._
import Defs._

case class ReferenceSchema(targetNamespace: Option[String],
                           topElems: Map[String, Tagged[XTopLevelElement]],
                           topTypes: Map[String, Tagged[XAnnotatedable]],
                           topAttrs: Map[String, Tagged[XTopLevelAttribute]],
                           topGroups: Map[String, Tagged[XNamedGroup]],
                           topAttrGroups: Map[String, Tagged[XNamedAttributeGroup]],
                           scope: scala.xml.NamespaceBinding,
                           unbound: XSchema)

object ReferenceSchema {
  def fromSchema(schema: XSchema, scope: scala.xml.NamespaceBinding): ReferenceSchema = {
    val ns = schema.targetNamespace map { _.toString }
    ReferenceSchema(ns,
      immutable.ListMap[String, Tagged[XTopLevelElement]](schema.xschemasequence1 collect {
        case DataRecord(_, _, x: XTopLevelElement) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      immutable.ListMap[String, Tagged[XAnnotatedable]](schema.xschemasequence1 collect {
        case DataRecord(_, _, x: XTopLevelSimpleType) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
        case DataRecord(_, _, x: XTopLevelComplexType) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      immutable.ListMap[String, Tagged[XTopLevelAttribute]](schema.xschemasequence1 collect {
        case DataRecord(_, _, x: XTopLevelAttribute) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      immutable.ListMap[String, Tagged[XNamedGroup]](schema.xschemasequence1 collect {
        case DataRecord(_, _, x: XNamedGroup) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      immutable.ListMap[String, Tagged[XNamedAttributeGroup]](schema.xschemasequence1 collect {
        case DataRecord(_, _, x: XNamedAttributeGroup) =>
          HostTag(ns, x).name -> Tagged(x, HostTag(ns, x))
      }: _*),
      scope,
      schema)
  }
}

case class WrappedSchema(targetNamespace: Option[String],
                         topElems: Map[String, Tagged[XTopLevelElement]],
                         topTypes: Map[String, Tagged[XAnnotatedable]],
                         topAttrs: Map[String, Tagged[XTopLevelAttribute]],
                         topGroups: Map[String, Tagged[XNamedGroup]],
                         topAttrGroups: Map[String, Tagged[XNamedAttributeGroup]],
                         elemList: List[Tagged[XElement]],
                         typeList: List[Tagged[XAnnotatedable]],
                         choices: List[Tagged[XGroup]],
                         attrList: List[Tagged[XAttributable]],
                         // annotation: Option[AnnotationDecl],
                         scope: scala.xml.NamespaceBinding) {

  val newline = System.getProperty("line.separator")

  override def toString: String = {
    "WrappedSchema(" + newline +
    "topElems(" + topElems.valuesIterator.mkString("," + newline) + ")," + newline +
    "topTypes(" + topTypes.valuesIterator.mkString("," + newline)  + ")," + newline +
    "topAttrs(" + topAttrs.valuesIterator.mkString("," + newline)  + ")," + newline +
    "topGroups(" + topGroups.valuesIterator.mkString("," + newline)  + ")," + newline +
    "topAttrGroups(" + topAttrGroups.valuesIterator.mkString("," + newline)  + ")" + newline +
    ")"
  }
}

object WrappedSchema {
  def typeList(schema: XSchema): Seq[Tagged[XAnnotatedable]] =
    schema flatMap {
      case Tagged(x, tag) => x match {
        case decl: XComplexType => List(Tagged(decl, tag))
        case decl: XSimpleType  => List(Tagged(decl, tag))
        case _ => Nil
      }
    }

  def elemList(schema: XSchema): Seq[Tagged[XElement]] =
    schema flatMap {
      case Tagged(x, tag) => x match {
        case elem: XElement => List(Tagged(elem, tag))
        case _ => Nil
      }
    }

  def attrList(schema: XSchema): Seq[Tagged[XAttributable]] =
    schema flatMap {
      case Tagged(x, tag) => x match {
        case attr: XAttributable => List(Tagged(attr, tag))
        case _ => Nil
      }
    }

  def choiceList(schema: XSchema): Seq[Tagged[XGroup]] =
    schema flatMap {
      case Tagged(x, tag) => x match {
        case KeyedGroup("choice", group) => List(Tagged(group, tag))
        case _ => Nil
      }
    }
}
