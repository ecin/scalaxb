import org.specs._

trait SpecBase extends Specification {
  val schemaString = """<schema targetNamespace="http://www.example.com/IPO"
        xmlns="http://www.w3.org/2001/XMLSchema"
        xmlns:ipo="http://www.example.com/IPO">

  <annotation>
    <documentation xml:lang="en">
      International Purchase order schema for Example.com
      Copyright 2000 Example.com. All rights reserved.
    </documentation>
  </annotation>

  <import namespace="http://www.w3.org/XML/1998/namespace"
      schemaLocation="http://www.w3.org/2001/xml.xsd"/>

  <element name="purchaseOrder" type="ipo:PurchaseOrderType"/>

  <element name="comment" type="string"/>

  <complexType name="PurchaseOrderType">
    <sequence>
      <element name="shipTo"     type="ipo:Address"/>
      <element name="billTo"     type="ipo:Address"/>
      <element ref="ipo:comment" minOccurs="0"/>
      <element name="items"      type="ipo:Items"/>
    </sequence>
    <attribute name="orderDate" type="date"/>
  </complexType>

  <complexType name="Items">
    <sequence>
      <element name="item" minOccurs="0" maxOccurs="unbounded">
        <complexType>
          <sequence>
            <element name="productName" type="string"/>
            <element name="quantity">
              <simpleType>
                <restriction base="positiveInteger">
                  <maxExclusive value="100"/>
                </restriction>
              </simpleType>
            </element>
            <element name="USPrice"    type="decimal"/>
            <element ref="ipo:comment" minOccurs="0"/>
            <element name="shipDate"   type="date" minOccurs="0"/>
          </sequence>
          <attribute name="partNum" type="ipo:SKU" use="required"/>
        </complexType>
      </element>
    </sequence>
  </complexType>

  <simpleType name="SKU">
    <restriction base="string">
      <pattern value="\d{3}-[A-Z]{2}"/>
    </restriction>
  </simpleType>

  <complexType name="Address">
    <sequence>
      <element name="name"   type="string"/>
      <element name="street" type="string"/>
      <element name="city"   type="string"/>
    </sequence>
  </complexType>

  <complexType name="USAddress">
    <complexContent>
      <extension base="ipo:Address">
        <sequence>
          <element name="state" type="ipo:USState"/>
          <element name="zip"   type="positiveInteger"/>
        </sequence>
      </extension>
    </complexContent>
  </complexType>

  <complexType name="UKAddress">
    <complexContent>
      <extension base="ipo:Address">
        <sequence>
          <element name="postcode" type="ipo:UKPostcode"/>
        </sequence>
        <attribute name="exportCode" type="positiveInteger" fixed="1"/>
      </extension>
    </complexContent>
  </complexType>

  <!-- other Address derivations for more countries -->

  <simpleType name="USState">
    <restriction base="string">
      <enumeration value="AK"/>
      <enumeration value="AL"/>
      <enumeration value="AR"/>

      <!-- and so on ... -->
      <enumeration value="DE"/>
    </restriction>
  </simpleType>

  <!-- simple type definition for UKPostcode -->
  <simpleType name="UKPostcode">
    <restriction base="string">
    </restriction>
  </simpleType>

  <element name="TopLevelElement">
    <complexType>
      <sequence>
        <choice maxOccurs="unbounded">
          <element name="foo" type="string"/>
          <any namespace="##other" processContents="lax" />
        </choice>
      </sequence>
    </complexType>
  </element>
</schema>"""
}
