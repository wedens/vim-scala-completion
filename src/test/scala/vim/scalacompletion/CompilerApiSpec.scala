package vim.scalacompletion

import org.specs2.mutable._
import java.io.{File => JFile}
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.interactive.Global

class CompilerApiSpec extends Specification {
  val rtJarPath = "/rt.jar"
  val scalaLibJarPath = "/scala-library-2.11.1.jar"
  val scalazJarPath = "/scalaz-core_2.11-7.0.6.jar"

  def jars = Seq(
    new JFile(getClass().getResource(rtJarPath).toURI), 
    new JFile(getClass().getResource(scalaLibJarPath).toURI),
    new JFile(getClass().getResource(scalazJarPath).toURI)
  )

  def createSource(code: String) = new BatchSourceFile("test", code)
  def locationMarker(str: String) = str.indexOf("$")
  def stripLocationMarker(codeRaw: String) = codeRaw.patch(locationMarker(codeRaw), "", 1)

  def createNameExtractorFor(compiler: Global): compiler.Member => String = 
    m => m.sym.nameString

  "compiler api" should {
    "add sources" in {
      val source = createSource("object app { println(\"x\") }")
      val compiler = CompilerFactory(jars)

      compiler.addSources(List(source)) must beLeft
    }

    "do type completion" in {
      val codeRaw = """object app extends App {
        val str = "some string"
        str.$
      }
      """
      val code = stripLocationMarker(codeRaw)
      val source = createSource(code)
      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))
      val nameExtractor = createNameExtractorFor(compiler)
      val position = source.position(locationMarker(codeRaw))

      compiler.typeCompletion(position, nameExtractor) must contain("substring")
    }

    "do scope completion" in {
      val codeRaw = """
      case class MyCaseClass(x: Int)
      object app extends App {
        $
      }
      """
      val code = stripLocationMarker(codeRaw)
      val source = createSource(code)
      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))
      val nameExtractor = createNameExtractorFor(compiler)
      val position = source.position(locationMarker(codeRaw))

      compiler.scopeCompletion(position, nameExtractor) must contain("MyCaseClass")
    }

    "remove sources" in {
      val source = createSource("object app { println(\"x\") }")
      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))

      compiler.removeSources(List(source)) must beLeft
    }
  }
}
