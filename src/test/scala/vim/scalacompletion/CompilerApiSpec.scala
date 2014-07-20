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

  def completionExample(exampleCode: => String) = {
    val codeRaw = s"""object app extends App {
      $exampleCode
    }
    """
    val code = stripLocationMarker(codeRaw)
    val source = createSource(code)
    val position = source.position(locationMarker(codeRaw))
    (position, source)
  }

  "compiler api" should {
    "add sources" in {
      val source = createSource("object app { println(\"x\") }")
      val compiler = CompilerFactory(jars)

      compiler.addSources(List(source)) must beLeft
    }

    "do type completion" in {
      val (position, source) = completionExample {
        """val str = "some string"
        str.$
        """
      }

      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))
      val nameExtractor = createNameExtractorFor(compiler)

      compiler.typeCompletion(position, nameExtractor) must contain("substring")
    }

    "do scope completion" in {
      val (position, source) = completionExample {
        """case class MyCaseClass(x: Int)
          $
        """
      }

      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))
      val nameExtractor = createNameExtractorFor(compiler)

      compiler.scopeCompletion(position, nameExtractor) must contain("MyCaseClass")
    }

    "do type completion inside of method call" in {
      val (position, source) = completionExample {
        """def add(x: Int, y: Int) = x + y
        val list = Seq(1, 2)
        add(list.$
        """
      }

      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))
      val nameExtractor = createNameExtractorFor(compiler)

      compiler.typeCompletion(position, nameExtractor) must contain("foldLeft")
    }

    "remove sources" in {
      val source = createSource("object app { println(\"x\") }")
      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))

      compiler.removeSources(List(source)) must beLeft
    }
  }
}
