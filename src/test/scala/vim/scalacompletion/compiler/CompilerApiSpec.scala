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
  def locationMarker(str: String) = str.indexOf("$")
  def stripLocationMarker(codeRaw: String) = codeRaw.patch(locationMarker(codeRaw), "", 1)

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

    "remove sources" in {
      val source = createSource("object app { println(\"x\") }")
      compiler.reloadSources(List(source))

      compiler.removeSources(List(source))
      ok
    }


    "" in {
      // Import(Ident(scalaz), List(ImportSelector(TermName("Monad"), 46, TermName("Monad"), 46), ImportSelector(termNames.ERROR, 53, termNames.ERROR, 53)))
      // val (position, source) = completionExample {
      //   """import scalaz.{Monad, $}"""
      // }


      // Apply(TypeApply(Select(Select(This(TypeName("app")), TermName("list")), TermName("map")), List(TypeTree(), TypeTree())), List())
      // val (position, source) = completionExample {
      //   """val list = Seq(1,2)
      //   list.map($)
      //   """
      // }



      // CaseDef(Apply(TypeTree().setOriginal(Select(Ident(scala), scala.Some)), List(Bind(TermName("value"), Ident(termNames.WILDCARD)))), Select(Ident(TermName("value")), [TermName("<error: value ==>") aka TermName("$eq$eq")]), Ident(TermName("value")))
      // val (position, source) = completionExample {
      //   """Option(3) match {
      //     case Some(value) if value == $ => value
      //   }
      //   """
      // }


      // Template(List(TypeTree(), TypeTree().setOriginal(Select(Ident(scala), scala.App))), noSelfType, List(DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(TypeName("app")), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))), ValDef(Modifiers(), TermName("list "), TypeTree(), Literal(Constant(1)))))
      // val (position, source) = completionExample {
      //   """
      //   val list = 1 $ 3 :: 4 :: Nil
      //   """
      // }

      // Apply(Select(Literal(Constant("some string")), TermName("$plus")), List(Select(Literal(Constant("another string")), <TermName("unary_$plus"): error>)))
      // val (position, source) = completionExample {
      //   """
      //   val str = "some string" + $ + "another string"
      //   """
      // }

      val (position, source) = completionExample {
        """
        class X extends Y with T {
          def f(x: Int) = x.$
        }
        """
      }

      println(compiler.askType(position))
      ok
    }
  }
}
