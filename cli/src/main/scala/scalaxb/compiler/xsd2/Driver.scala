/*
 * Copyright (c) 2011 e.e d3si9n
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

import java.io.{Reader, PrintWriter}
import java.net.{URI}
import scala.xml.{Node, Elem}
import scalaxb.compiler.{Module, Config, Snippet}
import scalaxb._
import Scalaxb._
import xmlschema._
import XDefaultXMLProtocol._

class Driver extends Module { driver =>
  import scala.xml.factory.{XMLLoader}
  import javax.xml.parsers.SAXParser

  type Schema = ReferenceSchema
  type Context = SchemaContext

  def generate(schema: Schema, context: Context, config: Config): Snippet =
    new Generator(schema, driver, config).generateEntitySource

  def generateProtocol(snippet: Snippet,
    context: Context, config: Config): Seq[Node] = Seq(<source/>)

  def toImportable(alocation: URI, ain: Reader, aout: PrintWriter): Importable = {
    new Importable {
      val reader: Reader = ain
      val out: PrintWriter = aout
      val location: URI = alocation
      lazy val elem = CustomXML.load(reader)

      def targetNamespace: Option[String] = None
      def importNamespaces: Seq[String] = Nil
      def importLocations: Seq[String] = Nil
      def includeLocations: Seq[String] = Nil

      def toSchema(context: Context): Schema = {
        val unbound = fromXML[XSchema](elem)
        // log("Driver.toSchema: " + unbound.toString())
        val wrapped = ReferenceSchema.fromSchema(unbound, elem.scope)
        log("Driver.toSchema: " + wrapped.toString)
        wrapped
      }
  }
  }

  def buildContext: Context = SchemaContext()

  def processContext(context: Context, config: Config) {
    // do nothing.
  }

  object CustomXML extends XMLLoader[Elem] {
    override def parser: SAXParser = {
      val factory = javax.xml.parsers.SAXParserFactory.newInstance()
      factory.setFeature("http://xml.org/sax/features/validation", false)
      factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", false)
      factory.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
      factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
      factory.newSAXParser()
    }
  }
}
