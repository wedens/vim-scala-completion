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
    new JFile(getClass().getResource(scalazJarPath).toURI),
    new JFile("/Users/wedens/.ivy2/cache/io.spray/spray-routing_2.11/bundles/spray-routing_2.11-1.3.1.jar"),
    new JFile("/Users/wedens/.ivy2/cache/com.chuusai/shapeless_2.11/jars/shapeless_2.11-1.2.4.jar"),
    new JFile("/Users/wedens/.ivy2/cache/com.typesafe.akka/akka-actor_2.11/jars/akka-actor_2.11-2.3.4.jar"),
    new JFile("/Users/wedens/.ivy2/cache/io.spray/spray-can_2.11/bundles/spray-can_2.11-1.3.1.jar"),
    new JFile("/Users/wedens/.ivy2/cache/io.spray/spray-io_2.11/bundles/spray-io_2.11-1.3.1.jar"),
    new JFile("/Users/wedens/.ivy2/cache/io.spray/spray-util_2.11/bundles/spray-util_2.11-1.3.1.jar"),
    new JFile("/Users/wedens/.ivy2/cache/io.spray/spray-http_2.11/bundles/spray-http_2.11-1.3.1.jar"),
    new JFile("/Users/wedens/.ivy2/cache/io.spray/spray-httpx_2.11/bundles/spray-httpx_2.11-1.3.1.jar")
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
        str$.
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
        """val list = Seq(1,2)
        def add(x: Int, y: Int): Int = x + y
        add(list$.)
        """
      }

      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))
      val nameExtractor = createNameExtractorFor(compiler)

      compiler.typeCompletion(position, nameExtractor) must contain("head")
    }

    "do type completion inside of method call with 2 params by name" in {
      val (position, source) = completionExample {
        """
        implicit class OptionW[T](opt: Option[T]) {
          def cata[A](some: T => A, none: A) = opt.map(some) getOrElse none
        }
        Option(List(1,2)).cata(_$.)"""
      }

      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))
      val nameExtractor = createNameExtractorFor(compiler)

      compiler.typeCompletion(position, nameExtractor) must contain("head")
    }

    "remove sources" in {
      val source = createSource("object app { println(\"x\") }")
      val compiler = CompilerFactory(jars)
      compiler.addSources(List(source))

      compiler.removeSources(List(source)) must beLeft
    }
  }
}
