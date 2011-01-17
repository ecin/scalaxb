import org.specs._
import scalaxb.compiler.{Verbose}

object IncTest extends SpecBase {
  val module = new scalaxb.compiler.xsd2.Driver // with Verbose
  // val module = new scalaxb.compiler.xsd.Driver // with Verbose

  lazy val generated = module.processString(schemaString, "general")
  lazy val entitySource = generated(0)

  "the generated entity source" should {
    "start with // Generated by" >> {
      println(entitySource)
      entitySource must startWith("// Generated by")
    }
  } // entity

  "top-level complex types" should {
    "generate a case class named similarly" >> {
      entitySource must include("case class Address(")
    }
  } // complexType


  "top-level simple types with enumeration" should {
    "generate a trait named similarly" >> {
      entitySource must include("trait MilkType")
    }

    "each enumerations represented as case object" >> {
      entitySource must include("case object SKIM")
    }
  } // enumeration

  "top-level elements with a local complex type" should {
    "generate a case class named similarly" >> {
      entitySource must include("case class TopLevelElementTest(")
    }
  } // element

  "local elements with a local complex type" should {
    "generate a case class named similarly" >> {
      entitySource must include("case class Item(")
    }
  } // local element

  "the generated case classes" should {
    "map xs:string params to String" >> {
      entitySource must find(
        """case class Address\(street: String,\s*
          |\s*city: String\)""".stripMargin)
    }

    val expectedSimpleTypeTest =
      """case class SimpleTypeTest\(milk1: MilkType,\s*
        |\s*milk2: MilkType,\s*
        |\s*quantity: BigInt,\s*
        |\s*comment: String,\s*
        |\s*comment2: String,\s*
        |\s*milklist1: Seq\[MilkType\],\s*
        |\s*union: String\)""".stripMargin

    "map simple type param with enumeration to the trait" >> {
      entitySource must find(expectedSimpleTypeTest)
    }

    "map simple type restriction of an enumeration to its base" >> {
      entitySource must find(expectedSimpleTypeTest)
    }

    "map simple type restriction of a built-in to the built-in" >> {
      entitySource must find(expectedSimpleTypeTest)
    }

    "map simple type restriction of a simple type to its base built-in" >> {
      entitySource must find(expectedSimpleTypeTest)
    }

    "map simple type restriction of an ad-hoc simple type to its base" >> {
      entitySource must find(expectedSimpleTypeTest)
    }

    "map list of a simple type as Seq" >> {
      entitySource must find(expectedSimpleTypeTest)
    }

    "map union of simple types as String" >> {
      entitySource must find(expectedSimpleTypeTest)
    }

    val expectedComplexTypeTest =
      """case class SingularComplexTypeTest\(person1: Person,\s*
        |\s*person2: Option\[Person\],\s*
        |\s*person3: Option\[Person\],\s*
        |\s*person4: Option\[Option\[Person\]\],\s*
        |\s*person5: Seq\[Person\],\s*
        |\s*person6: Seq\[Option\[Person\]\]\)""".stripMargin

    "map complex type param to the class/trait" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "map nillable complex type param to Option[A]" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "map optional complex type param to Option[A]" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "map nillable optional complex type param to Option[Option[A]]" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "map maxOccurs >1 complex type param to Seq[A]" >> {
      entitySource must find(expectedComplexTypeTest)
    }

    "map nillable maxOccurs >1 complex type param to Seq[Option[A]]" >> {
      entitySource must find(expectedComplexTypeTest)
    }
  } // generated case classes
}
