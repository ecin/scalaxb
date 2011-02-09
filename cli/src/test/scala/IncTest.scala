import org.specs._
import scalaxb.compiler.{Verbose}

object IncTest extends Specification {
  val module = new scalaxb.compiler.xsd2.Driver

  "the generated entity source" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
      xmlns:xs="http://www.w3.org/2001/XMLSchema" />, "example")(0)

    "start with // Generated by" >> {
      println(entitySource)
      entitySource must startWith("// Generated by")
    }
  } // entity


  "xs:string" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "be referenced as String" >> {
      println(entitySource)
      entitySource must find(
        """case class Address\(street: String,\s*
          |\s*city: String\)""".stripMargin)
    }
  }

  "restrictions of xs:positiveInteger" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="quantity">
            <xs:simpleType>
              <xs:restriction base="xs:positiveInteger">
                <xs:maxExclusive value="100"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "be referenced as BigInt" >> {
      println(entitySource)
      entitySource must include("""quantity: BigInt""")
    }
  } // xs:positiveInteger restriction

  "restrictions of simple type" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:simpleType name="ShortString">
        <xs:restriction base="xs:string">
          <xs:maxLength value="140"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="comment">
            <xs:simpleType>
              <xs:restriction base="gen:ShortString">
                <xs:maxLength value="100"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
          <xs:element name="comment2">
            <xs:simpleType>
              <xs:restriction>
                <xs:simpleType>
                  <xs:restriction base="gen:ShortString">
                    <xs:maxLength value="130"/>
                  </xs:restriction>
                </xs:simpleType>
                <xs:maxLength value="100"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "be referenced as its base built-in type" >> {
      println(entitySource)
      entitySource must include("""comment: String""")
      entitySource must include("""comment2: String""")
    }
  } // restriction restriction

  "lists of a simple type" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="milklist1" type="gen:ListOfMilk"/>
        </xs:sequence>
      </xs:complexType>

      <xs:simpleType name="ListOfMilk">
        <xs:list itemType="gen:MilkType"/>
      </xs:simpleType>

      <xs:simpleType name="MilkType">
        <xs:restriction base="xs:NMTOKEN">
          <xs:enumeration value="WHOLE"/>
          <xs:enumeration value="SKIM"/>
        </xs:restriction>
      </xs:simpleType>
    </xs:schema>, "example")(0)

    "be referenced as Seq" >> {
      println(entitySource)
      entitySource must include("""milklist1: Seq[MilkType]""")
    }
  } // list

  "unions of simple types" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="union">
            <xs:simpleType>
              <xs:union memberTypes="xs:string xs:int" />
            </xs:simpleType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "be referenced as String" >> {
      println(entitySource)
      entitySource must include("""union: String""")
    }
  } // union

  "top-level simple types with enumeration" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <xs:simpleType name="MilkType">
        <xs:restriction base="xs:NMTOKEN">
          <xs:enumeration value="WHOLE"/>
          <xs:enumeration value="SKIM"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="milk1" type="gen:MilkType"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "generate a trait named similarly" >> {
      println(entitySource)
      entitySource must include("""trait MilkType""")
    }

    "each enumerations represented as case object" >> {
      entitySource must include("""case object SKIM""")
    }

    "be referenced as the trait" >> {
      entitySource must include("""milk1: MilkType""")
    }
  } // enumeration

  "restrictions of simple types with enumeration" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <xs:simpleType name="MilkType">
        <xs:restriction base="xs:NMTOKEN">
          <xs:enumeration value="WHOLE"/>
          <xs:enumeration value="SKIM"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:complexType name="SimpleTypeTest">
        <xs:sequence>
          <xs:element name="milk2">
            <xs:simpleType>
              <xs:restriction base="gen:MilkType">
                <xs:enumeration value="WHOLE"/>
              </xs:restriction>
            </xs:simpleType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "not generate its own trait" >> {
      println(entitySource)
      entitySource mustNot find("""SimpleTypeTestWHOLE""")
    }

    "be referenced as its base trait" >> {
      entitySource must find("""milk2: MilkType""")
    }
  } // enum restriction

  "top-level complex types" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Person">
        <xs:sequence>
          <xs:element name="firstName" type="xs:string"/>
          <xs:element name="lastName" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="SingularComplexTypeTest">
        <xs:sequence>
          <xs:element name="person1" type="gen:Person"/>
          <xs:element name="person2" nillable="true" type="gen:Person"/>
          <xs:element name="person3" minOccurs="0" type="gen:Person"/>
          <xs:element name="person4" minOccurs="0" nillable="true" type="gen:Person"/>
          <xs:element name="person5" maxOccurs="unbounded" type="gen:Person"/>
          <xs:element name="person6" maxOccurs="unbounded" nillable="true" type="gen:Person"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "generate a case class named similarly" >> {
      println(entitySource)
      entitySource must include("""case class Address(""")
    }

    "not generate case class for the primary sequence" >> {
      entitySource must not include("""AddressSequence""")
    }

    val expectedComplexTypeTest =
      """case class SingularComplexTypeTest\(person1: Person,\s*
        |\s*person2: Option\[Person\],\s*
        |\s*person3: Option\[Person\],\s*
        |\s*person4: Option\[Option\[Person\]\],\s*
        |\s*person5: Seq\[Person\],\s*
        |\s*person6: Seq\[Option\[Person\]\]\)""".stripMargin

    "be referenced as the class/trait" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "be referenced as Option[A] if nillable" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "be referenced as Option[A] if optional" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "be referenced as Option[Option[A]] if nillable and optional" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "be referenced as Seq[A] if maxOccurs >1" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "be referenced as Seq[Option[A]] if nillable and maxOccurs >1" >> {
      entitySource must find(expectedComplexTypeTest)
    }
  } // complexType

  "top-level elements with a local complex type" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/"
        xmlns:xs="http://www.w3.org/2001/XMLSchema">
      <xs:element name="topLevelElementTest">
        <xs:complexType>
          <xs:sequence>
            <xs:choice maxOccurs="unbounded">
              <xs:element name="foo" type="xs:string"/>
              <xs:any namespace="##other" processContents="lax" />
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:schema>, "example")(0)

    "generate a case class named similarly" >> {
      println(entitySource)
      entitySource must include("""case class TopLevelElementTest(""")
    }
  } // element

  "local elements with a local complex type" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/ipo"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:ipo="http://www.example.com/ipo">
      <xs:element name="comment" type="xs:string"/>

      <xs:complexType name="Items">
        <xs:sequence>
          <xs:element name="item" minOccurs="0" maxOccurs="unbounded">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="productName" type="xs:string"/>
                <xs:element name="quantity">
                  <xs:simpleType>
                    <xs:restriction base="xs:positiveInteger">
                      <xs:maxExclusive value="100"/>
                    </xs:restriction>
                  </xs:simpleType>
                </xs:element>
                <xs:element name="USPrice"    type="xs:decimal"/>
                <xs:element ref="ipo:comment" minOccurs="0"/>
                <xs:element name="shipDate"   type="xs:date" minOccurs="0"/>
              </xs:sequence>
              <xs:attribute name="partNum" type="xs:int" use="required"/>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "generate a case class named similarly" >> {
      println(entitySource)
      entitySource must include("""case class Item(""")
    }
  } // local element

  "choices in a complex type" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="ChoiceComplexTypeTest">
        <xs:sequence>
          <xs:choice>
            <xs:element name="person1" type="gen:Person"/>
            <xs:element name="address1" type="gen:Address"/>
          </xs:choice>
          <xs:choice>
            <xs:element name="person2" nillable="true" type="gen:Person"/>
            <xs:element name="address2" nillable="true" type="gen:Address"/>
          </xs:choice>
          <xs:choice minOccurs="0">
            <xs:element name="person3" type="gen:Person"/>
            <xs:element name="address3" type="gen:Address"/>
          </xs:choice>
          <xs:choice minOccurs="0">
            <xs:element name="person4" nillable="true" type="gen:Person"/>
            <xs:element name="address4" nillable="true" type="gen:Address"/>
          </xs:choice>
          <xs:choice maxOccurs="unbounded">
            <xs:element name="person5" type="gen:Person"/>
            <xs:element name="address5" type="gen:Address"/>
          </xs:choice>
          <xs:choice maxOccurs="unbounded">
            <xs:element name="person6" nillable="true" type="gen:Person"/>
            <xs:element name="address6" nillable="true" type="gen:Address"/>
          </xs:choice>
          <xs:choice>
            <xs:element name="int1" type="xs:int"/>
            <xs:element name="int2" type="xs:int"/>
          </xs:choice>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Person">
        <xs:sequence>
          <xs:element name="firstName" type="xs:string"/>
          <xs:element name="lastName" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>

      <xs:complexType name="Address">
        <xs:sequence>
          <xs:element name="street" type="xs:string"/>
          <xs:element name="city" type="xs:string"/>
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    "generate a trait named FooOption*" >> {
      println(entitySource)
      entitySource must include("""trait ChoiceComplexTypeTestOption""")
    }

    val expectedChoiceTest =
      """case class ChoiceComplexTypeTest\(choicecomplextypetestoption: scalaxb.DataRecord\[ChoiceComplexTypeTestOption\],\s*
        |\s*choicecomplextypetestoption2: scalaxb.DataRecord\[Option\[ChoiceComplexTypeTestOption2\]\],\s*
        |\s*choicecomplextypetestoption3: Option\[scalaxb.DataRecord\[ChoiceComplexTypeTestOption3\]\],\s*
        |\s*choicecomplextypetestoption4: Option\[scalaxb.DataRecord\[Option\[ChoiceComplexTypeTestOption4\]\]\],\s*
        |\s*choicecomplextypetestoption5: Seq\[scalaxb.DataRecord\[ChoiceComplexTypeTestOption5\]\],\s*
        |\s*choicecomplextypetestoption6: Seq\[scalaxb.DataRecord\[Option\[ChoiceComplexTypeTestOption6\]\]\],\s*
        |\s*choicecomplextypetestoption7: scalaxb.DataRecord\[Int\]\)\s*
        |""".stripMargin

    "be referenced as DataRecord[FooOption] if it's made of non-nillable complex type element" >> {
      entitySource must find(expectedChoiceTest)
    }

    "be referenced as DataRecord[Option[FooOption]] if it's made of complex types, some nillable" >> {
      entitySource must find(expectedChoiceTest)
    }

    "be referenced as DataRecord[Int] if it's made of xs:int" >> {
      entitySource must find(expectedChoiceTest)
    }
  } // choices

  "wildcard" should {
    val entitySource = module.processNode(<xs:schema targetNamespace="http://www.example.com/general"
        xmlns:xs="http://www.w3.org/2001/XMLSchema"
        xmlns:gen="http://www.example.com/general">
      <xs:complexType name="AnyTest2">
        <xs:sequence>
          <xs:any minOccurs="0" />
          <xs:element name="foo" type="xs:string" />
          <xs:any/>
          <xs:element name="foo2" type="xs:string" />
          <xs:any maxOccurs="unbounded" />
        </xs:sequence>
      </xs:complexType>
    </xs:schema>, "example")(0)

    val exptectedAnyTest =
      """case class AnyTest2\(any: Option\[scalaxb\.DataRecord\[Any\]\],\s*
        |\s*foo: String,\s*
        |\s*any2: scalaxb\.DataRecord\[Any\],\s*
        |\s*foo2: String,\s*
        |\s*any3: Seq\[scalaxb\.DataRecord\[Any\]\]\)""".stripMargin

    "be referenced as DataRecord[Any] named any*" >> {
      println(entitySource)
      entitySource must find(exptectedAnyTest)
    }

    "be referenced as Option[DataRecord[A]] if optional" >> {
      entitySource must find(exptectedAnyTest)
    }

    "be referenced as Seq[DataRecord[A]] if maxOccurs >1" >> {
      entitySource must find(exptectedAnyTest)
    }
  } // generated case classes
}
