package vim.scalacompletion.compiler

import java.io.{File => JFile}
import org.specs2.mutable._
import scala.reflect.internal.util.BatchSourceFile

class CompilerApiSpec extends Specification {
  val rtJarPath = "/rt.jar"
  val scalaLibJarPath = "/scala-library-2.11.1.jar"
  val scalazJarPath = "/scalaz-core_2.11-7.0.6.jar"

  // args(skipAll = true)

  def jars = Seq(
    new JFile(getClass.getResource(rtJarPath).toURI),
    new JFile(getClass.getResource(scalaLibJarPath).toURI),
    new JFile(getClass.getResource(scalazJarPath).toURI)
  )

  val compilerFactory = new CompilerFactoryImpl()
  var compiler: Compiler = compilerFactory.create(jars)
  val nameExtractor: Compiler#Member => String = m => m.sym.nameString

  def createSource(code: String) = new BatchSourceFile("test", code)
  def locationMarker(str: String, marker: String = "$") = str.indexOf(marker)
  def stripLocationMarker(codeRaw: String, marker: String = "$") =
    codeRaw.patch(locationMarker(codeRaw, marker), "", 1)

  def completionExample(exampleCode: => String) = {
    val codeRaw = s"""object app extends App {
      $exampleCode
    }
    """
    val code = stripLocationMarker(codeRaw)
    val source = createSource(code)
    compiler.reloadSources(List(source))
    val position = source.position(locationMarker(codeRaw))
    (position, source)
  }

  sequential

  "compiler api" should {
    "add sources" in {
      val source = createSource("object app { println(\"x\") }")

      compiler.reloadSources(List(source))
      ok
    }

    "return members of local variable on type completion" in {
      val (position, source) = completionExample {
        """val str = "some string"
        str$.
        """
      }

      compiler.typeCompletion(position, nameExtractor) must contain("substring")
    }

    "return locally defined class on scope completion" in {
      val (position, source) = completionExample {
        """case class MyCaseClass(x: Int)
          $
        """
      }

      compiler.scopeCompletion(position, nameExtractor) must contain("MyCaseClass")
    }

    "return type members on type completion in method call" in {
      val (position, source) = completionExample {
        """val list = Seq(1,2)
        def add(x: Int, y: Int): Int = x + y
        add(list$.)
        """
      }

      compiler.typeCompletion(position, nameExtractor) must contain("head")
    }

    "return type members on method call with incomplete argument list" in pending {
      val (position, source) = completionExample {
        """
        implicit class OptionW[T](opt: Option[T]) {
          def cata[A](some: T => A, none: A) = opt.map(some) getOrElse none
        }
        Option(List(1,2)).cata(some = l => l$.)"""
      }

      compiler.typeCompletion(position, nameExtractor) must contain("head")
    }

    "return scope members after infix call" in {
      val (position, _) = completionExample {
        """
        def idInt(x: Int) = x
        val list = List(1, 2)
        list map$
        """
      }

      compiler.scopeCompletion(position, nameExtractor) must contain("idInt")
    }

    "return package members on type completion in imports" in {
      val (position, source) = completionExample {
        """
        import scalaz.{Monad, $}
        """
      }

      compiler.typeCompletion(position, nameExtractor) must contain("Monoid")
    }

    "getTypeAt test" in {
      // val (position, source) = completionExample {
      //   """
      //   "lalalalala $"
      //   """
      // }
      val codeRaw = """object app extends App {
    val list = Seq(1,2)
    def add(x: Int, y: Int) = x + y
    add(list.%())
    val x = 5
    println("" + x)
}
      """
      val code = stripLocationMarker(codeRaw, "%")
      val source = createSource(code)
      compiler.reloadSources(List(source))
      val position = source.position(locationMarker(codeRaw, "%"))
      println("Type column: " + position.column)
      println("Char:" + source.content(position.point))

      val tree = compiler.getTypeAt(position)
      println(compiler.showRaw(tree))

      val completionPos = position.withPoint(position.point - 1)
      println("Completion column: " + completionPos.column)
      compiler.typeCompletion(completionPos, nameExtractor) must contain("flatMap")

      ok
    }

    "remove sources" in {
      val source = createSource("object app { println(\"x\") }")
      compiler.reloadSources(List(source))

      compiler.removeSources(List(source))
      ok
    }
  }
}
