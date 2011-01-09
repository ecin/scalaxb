import org.specs._
import java.io.{File}
import scalaxb.compiler.{Module, Verbose}

trait TestBase extends Specification with CompilerMatcher {
  val module: Module = new scalaxb.compiler.xsd2.Driver // with Verbose
  val tmp = new File("tmp")
  if (tmp.exists)
    deleteAll(tmp)
  tmp.mkdir
}
