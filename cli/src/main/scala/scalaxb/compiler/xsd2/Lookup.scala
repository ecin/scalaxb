package scalaxb.compiler.xsd2

import xmlschema._

trait Lookup {
  def typeName(decl: Tagged[XComplexType]) =
    FullName(None, decl.tag.name)
}

case class FullName(packageName: Option[String], localName: String) {
}
