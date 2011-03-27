import org.specs._

trait SpecBase extends Specification {
  val schemaString = """<?xml version='1.0' encoding='UTF-8'?>
<xs:schema targetNamespace="http://www.example.com/general"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:gen="http://www.example.com/general">
  <xs:complexType name="SingularBuiltInTypeTest">
    <xs:sequence>
      <xs:element name="int" type="xs:int"/>
      <xs:element name="byte" type="xs:byte"/>
      <xs:element name="short" type="xs:short"/>
      <xs:element name="long" type="xs:long"/>
      <xs:element name="float" type="xs:float"/>
      <xs:element name="double" type="xs:double"/>
      <xs:element name="integer" type="xs:integer"/>
      <xs:element name="nonPositiveInteger" type="xs:nonPositiveInteger"/>
      <xs:element name="negativeInteger" type="xs:negativeInteger"/>
      <xs:element name="nonNegativeInteger" type="xs:nonNegativeInteger"/>
      <xs:element name="positiveInteger" type="xs:positiveInteger"/>
      <xs:element name="unsignedLong" type="xs:unsignedLong"/>
      <xs:element name="unsignedInt" type="xs:unsignedInt"/>
      <xs:element name="unsignedShort" type="xs:unsignedShort"/>
      <xs:element name="unsignedByte" type="xs:unsignedByte"/>
      <xs:element name="decimal" type="xs:decimal"/>
      <xs:element name="boolean" type="xs:boolean"/>
      <xs:element name="string" type="xs:string"/>
      <xs:element name="normalizedString" type="xs:normalizedString"/>
      <xs:element name="token" type="xs:token"/>
      <xs:element name="language" type="xs:language"/>
      <xs:element name="Name" type="xs:Name"/>
      <xs:element name="NCName" type="xs:NCName"/>
      <xs:element name="NMTOKEN" type="xs:NMTOKEN"/>
      <xs:element name="NMTOKENS" type="xs:NMTOKENS"/>
      <xs:element name="ID" type="xs:ID"/>
      <xs:element name="IDREF" type="xs:IDREF"/>
      <xs:element name="IDREFS" type="xs:IDREFS"/>
      <xs:element name="ENTITY" type="xs:ENTITY"/>
      <xs:element name="ENTITIES" type="xs:ENTITIES"/>
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

  <xs:complexType name="NillablePersonStarTest">
    <xs:sequence>
      <xs:element name="person" maxOccurs="unbounded" nillable="true" type="gen:Person"/>
    </xs:sequence>
  </xs:complexType>

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
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="SingularSimpleTypeTest">
    <xs:sequence>
      <xs:element name="number1" type="xs:unsignedInt"/>
      <xs:element name="number2" nillable="true" type="xs:unsignedInt"/>
      <xs:element name="number3" minOccurs="0" type="xs:unsignedInt"/>
      <xs:element name="number4" minOccurs="0" nillable="true" type="xs:unsignedInt"/>
      <xs:element name="number5" maxOccurs="unbounded" type="xs:unsignedInt"/>
      <xs:element name="number6" maxOccurs="unbounded" nillable="true" type="xs:unsignedInt"/>

      <xs:element name="milk1" type="gen:MilkType"/>
      <xs:element name="milk2" nillable="true" type="gen:MilkType"/>
      <xs:element name="milk3" minOccurs="0" type="gen:MilkType"/>
      <xs:element name="milk4" minOccurs="0" nillable="true" type="gen:MilkType"/>
      <xs:element name="milk5" maxOccurs="unbounded" type="gen:MilkType"/>
      <xs:element name="milk6" maxOccurs="unbounded" nillable="true" type="gen:MilkType"/>
    </xs:sequence>

    <xs:attribute name="attr1" type="xs:unsignedInt"/>
    <xs:attribute name="attr2" type="gen:MilkType"/>
  </xs:complexType>

  <xs:complexType name="ListTest">
    <xs:sequence>
      <xs:element name="numbers1" type="gen:ListOfBuiltInType"/>
      <xs:element name="numbers2" nillable="true" type="gen:ListOfBuiltInType"/>
      <xs:element name="numbers3" minOccurs="0" type="gen:ListOfBuiltInType"/>
      <xs:element name="numbers4" minOccurs="0" nillable="true" type="gen:ListOfBuiltInType"/>
      <xs:element name="numbers5" maxOccurs="unbounded" type="gen:ListOfBuiltInType"/>
      <xs:element name="numbers6" maxOccurs="unbounded" nillable="true" type="gen:ListOfBuiltInType"/>

      <xs:element name="milk1" type="gen:ListOfMilk"/>
      <xs:element name="milk2" nillable="true" type="gen:ListOfMilk"/>
      <xs:element name="milk3" minOccurs="0" type="gen:ListOfMilk"/>
      <xs:element name="milk4" minOccurs="0" nillable="true" type="gen:ListOfMilk"/>
      <xs:element name="milk5" maxOccurs="unbounded" type="gen:ListOfMilk"/>
      <xs:element name="milk6" maxOccurs="unbounded" nillable="true" type="gen:ListOfMilk"/>
    </xs:sequence>

    <xs:attribute name="attr1" type="gen:ListOfBuiltInType"/>
    <xs:attribute name="attr2" type="gen:ListOfMilk"/>
  </xs:complexType>

  <xs:complexType name="AnyTest">
    <xs:sequence>
      <xs:element name="person1" type="gen:Person"/>
      <xs:any namespace="##other" processContents="lax"/>
      <xs:choice>
        <xs:element name="person2" nillable="true" type="gen:Person"/>
        <xs:any namespace="##other" processContents="lax"/>
      </xs:choice>
      <xs:element name="person3" minOccurs="0" type="gen:Person"/>
      <xs:any namespace="##other" processContents="lax" minOccurs="0"/>
      <xs:choice>
        <xs:element name="person4" minOccurs="0" nillable="true" type="gen:Person"/>
        <xs:any namespace="##other" processContents="lax" minOccurs="0"/>
      </xs:choice>
      <xs:element name="person5" maxOccurs="unbounded" type="gen:Person"/>
      <xs:any namespace="##other" processContents="lax" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="AnyTypeTest">
    <xs:sequence>
      <xs:element name="any1" type="xs:anyType"/>
      <xs:element name="any2" nillable="true" type="xs:anyType"/>
      <xs:element name="any3" minOccurs="0" type="xs:anyType"/>
      <xs:element name="any4" minOccurs="0" nillable="true" type="xs:anyType"/>
      <xs:element name="any5" maxOccurs="unbounded" type="xs:anyType"/>
      <xs:element name="any6" maxOccurs="unbounded" nillable="true" type="xs:anyType"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="LongAllTest">
    <xs:all>
      <xs:element name="address1" type="gen:Address"/>
      <xs:element name="address2" minOccurs="0" type="gen:Address"/>
      <xs:element name="string3" type="xs:string"/>
      <xs:element name="string4" type="xs:string"/>
      <xs:element name="string5" type="xs:string"/>
      <xs:element name="string6" type="xs:string"/>
      <xs:element name="string7" type="xs:string"/>
      <xs:element name="string8" type="xs:string"/>
      <xs:element name="string9" type="xs:string"/>
      <xs:element name="string10" type="xs:string"/>
      <xs:element name="string11" type="xs:string"/>
      <xs:element name="string12" type="xs:string"/>
      <xs:element name="string13" type="xs:string"/>
      <xs:element name="string14" type="xs:string"/>
      <xs:element name="string15" type="xs:string"/>
      <xs:element name="string16" type="xs:string"/>
      <xs:element name="string17" type="xs:string"/>
      <xs:element name="string18" type="xs:string"/>
      <xs:element name="string19" type="xs:string"/>
      <xs:element name="string20" type="xs:string"/>
      <xs:element name="string21" type="xs:string"/>
      <xs:element name="string22" type="xs:string"/>
      <xs:element name="string23" type="xs:string"/>
      <xs:element name="string24" type="xs:string"/>
      <xs:element name="string25" type="xs:string"/>
      <xs:element name="string26" type="xs:string"/>
      <xs:element name="string27" type="xs:string"/>
      <xs:element name="string28" type="xs:string"/>
      <xs:element name="string29" type="xs:string"/>
      <xs:element name="string30" type="xs:string"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="TopLevelMultipleSeqTest">
    <xs:sequence minOccurs="0" maxOccurs="unbounded">
      <xs:element name="firstName" type="xs:string"/>
      <xs:element name="lastName" type="xs:string"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="TopLevelOptionalSeqTest">
    <xs:sequence minOccurs="0">
      <xs:element name="firstName" type="xs:string"/>
      <xs:element name="lastName" type="xs:string"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="TopLevelMultipleSeqAnyTest" mixed="true">
    <xs:sequence minOccurs="0" maxOccurs="unbounded">
      <xs:any namespace="##other" processContents="lax"/>
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

  <xs:simpleType name="ListOfMilk">
    <xs:list itemType="gen:MilkType"/>
  </xs:simpleType>

  <xs:simpleType name="MilkType">
    <xs:restriction base="xs:NMTOKEN">
      <xs:enumeration value="WHOLE"/>
      <xs:enumeration value="SKIM"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="ListOfBuiltInType">
    <xs:list itemType="xs:unsignedInt"/>
  </xs:simpleType>

  <xs:complexType name="DupeSequenceTest">
    <xs:sequence>
      <xs:element name="street" type="xs:string"/>
			<xs:element name="other" minOccurs="0">
				<xs:complexType>
					<xs:sequence>
						<xs:any namespace="##any" processContents="lax" maxOccurs="unbounded"/>
					</xs:sequence>
				</xs:complexType>
			</xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="DupeSequenceTest2">
    <xs:sequence>
      <xs:element name="street" type="xs:string"/>
			<xs:element name="other" minOccurs="0">
				<xs:complexType>
					<xs:sequence>
						<xs:any namespace="##any" processContents="lax" maxOccurs="unbounded"/>
					</xs:sequence>
				</xs:complexType>
			</xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:element name="topLevelElementTest">
    <xs:complexType>
      <xs:sequence>
        <xs:choice maxOccurs="unbounded">
          <xs:element name="foo" type="string"/>
          <xs:any namespace="##other" processContents="lax" />
        </xs:choice>
      </xs:sequence>
    </xs:complexType>
  </xs:element>

  <xs:element name="longAttributeTest">
    <xs:complexType>
      <xs:attribute name="milk1" type="gen:MilkType"/>
      <xs:attribute name="string2" type="xs:string"/>
      <xs:attribute name="string3" type="xs:string"/>
      <xs:attribute name="string4" type="xs:string"/>
      <xs:attribute name="string5" type="xs:string"/>
      <xs:attribute name="string6" type="xs:string"/>
      <xs:attribute name="string7" type="xs:string"/>
      <xs:attribute name="string8" type="xs:string"/>
      <xs:attribute name="string9" type="xs:string"/>
      <xs:attribute name="string10" type="xs:string"/>
      <xs:attribute name="string11" type="xs:string"/>
      <xs:attribute name="string12" type="xs:string"/>
      <xs:attribute name="string13" type="xs:string"/>
      <xs:attribute name="string14" type="xs:string"/>
      <xs:attribute name="string15" type="xs:string"/>
      <xs:attribute name="string16" type="xs:string"/>
      <xs:attribute name="string17" type="xs:string"/>
      <xs:attribute name="string18" type="xs:string"/>
      <xs:attribute name="string19" type="xs:string"/>
      <xs:attribute name="string20" type="xs:string"/>
      <xs:attribute name="string21" type="xs:string"/>
      <xs:attribute name="string22" type="xs:string"/>
      <xs:attribute name="string23" type="xs:string"/>
      <xs:attribute name="string24" type="xs:string"/>
      <xs:attribute name="string25" type="xs:string"/>
      <xs:attribute name="string26" type="xs:string"/>
      <xs:attribute name="string27" type="xs:string"/>
      <xs:attribute name="string28" type="xs:string"/>
      <xs:attribute name="string29" type="xs:string"/>
      <xs:attribute name="string30" type="xs:string"/>
      <xs:attributeGroup ref="gen:coreattrs"/>
      <xs:anyAttribute namespace="##any"/>
    </xs:complexType>
  </xs:element>

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
            <xs:element ref="gen:comment" minOccurs="0"/>
            <xs:element name="shipDate"   type="xs:date" minOccurs="0"/>
          </xs:sequence>
          <xs:attribute name="partNum" type="xs:int" use="required"/>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:simpleType name="ShortString">
    <xs:restriction base="xs:string">
      <xs:maxLength value="140"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="SimpleTypeTest">
    <xs:sequence>
      <xs:element name="milk1" type="gen:MilkType"/>
      <xs:element name="milk2">
        <xs:simpleType>
          <xs:restriction base="gen:MilkType">
            <xs:enumeration value="WHOLE"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element name="quantity">
        <xs:simpleType>
          <xs:restriction base="xs:positiveInteger">
            <xs:maxExclusive value="100"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
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
      <xs:element name="milklist1" type="gen:ListOfMilk"/>
      <xs:element name="union">
        <xs:simpleType>
          <xs:union memberTypes="xs:string xs:int" />
        </xs:simpleType>
      </xs:element>

    </xs:sequence>
  </xs:complexType>

  <xs:attributeGroup name="coreattrs">
    <xs:annotation>
      <xs:documentation>
      core attributes common to most elements
      id       document-wide unique id
      </xs:documentation>
    </xs:annotation>
    <xs:attribute name="id" type="xs:ID"/>
    <xs:attribute name="class" type="xs:NMTOKENS"/>
  </xs:attributeGroup>
</xs:schema>"""
}
