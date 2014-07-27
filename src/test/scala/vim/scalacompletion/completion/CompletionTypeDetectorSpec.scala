package vim.scalacompletion.completion

import org.specs2.mutable._

class CompletionTypeDetectorSpec extends Specification {
  val detector = new CompletionTypeDetector

  "completion type detector" should {
    "detect type completion after ." in {
      val line = "str. "

      detector.detect(line, line.length - 1) must_== CompletionType.Type
    }

    "detect scope completion on blank line" in  {
      val line = " " * 5

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope completion after ;" in {
      val line = "str.toString(); "

      detector.detect(line, line.indexOf(" ")) must_== CompletionType.Scope
    }

    "detect type completion after word with following spaces" in {
      val line = "str      "

      detector.detect(line, line.length - 1) must_== CompletionType.Type
    }

    "detect type completion after" in {
      val line = "str "

      detector.detect(line, line.length - 1) must_== CompletionType.Type
    }

    "detect type completion after word with preceeding spaces" in {
      val line = "   str  "

      detector.detect(line, line.length - 1) must_== CompletionType.Type
    }

    "detect scope completion after infix method call" in {
      val line = "someVar |@|  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope completion after math symbol infix method call" in {
      val line = "someVar ∘∘  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope completion after infix method call with preceeding colon" in {
      val line = "f(); actor !  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope completion after infix method call" in {
      val line = "      matches flatMap  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope completion after infix unicode method call" in {
      val line = "сосиска положитьВ  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect type completion after unicode identifier" in {
      val line = "сосиска "

      detector.detect(line, line.length - 1) must_== CompletionType.Type
    }

    "detect scope completion iside of []" in {
      val line = "type X[T] = Either[T,  ]"

      detector.detect(line, line.indexOf(",") + 2) must_== CompletionType.Scope
    }

    "detect scope completion inside function arguments" in {
      val line = "list.map( )"

      detector.detect(line, line.indexOf(" ")) must_== CompletionType.Scope
    }

    "detect type completion in the middle of expression" in {
       val line = "f().  match"

       detector.detect(line, line.indexOf(".") + 1) must_== CompletionType.Type
    }

    "detect type completion in the middle of expression with infix operator" in {
       val line = "val list = x   y :: z :: Nil"

       detector.detect(line, line.indexOf("x") + 2) must_== CompletionType.Type
    }

    "detect scope completion in the middle of type expression" in {
      val line = "type X[T] = ValidationNel[ , Option[T]]"

      detector.detect(line, line.indexOf(",") - 1) must_== CompletionType.Scope
    }

    "detect scope in the middle of case expression" in {
      val line = "  case Some(value) if value ==   => value"

      detector.detect(line, line.indexOf("=>") - 2) must_== CompletionType.Scope
    }

    "detect scope in between 'case' and '=>'" in {
      val line = "  case   =>"

      detector.detect(line, line.indexOf("=>") - 2) must_== CompletionType.Scope
    }

    "detect scope after 'new' keyword" in {
      val line = "val x = new  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after 'extends' keyword" in {
      val line = "class MyClass extends  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after 'class' keyword" in {
      val line = "class  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after 'trait' keyword" in {
      val line = "trait  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after 'val' keyword" in {
      val line = "val  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after 'var' keyword" in {
      val line = "var  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after 'def' keyword" in {
      val line = "def  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope in between extends statement and other code" in {
      val line = "class MyClass extends   with Log"

      detector.detect(line, line.indexOf("extends") + 2) must_== CompletionType.Scope
    }

    "detect scope after with statement" in {
      val line = "class MyClass extends Base with  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after yield statement" in {
      val line = "for (x <- y) yield  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "not detect completion after dot inside string" in {
       val line = "val str = \"some. string\""

       detector.detect(line, line.indexOf(".")) must_== CompletionType.NoCompletion
    }

    "detect scope completion in string concatenation" in {
       val line = "val str = \"some string\" +   + \"another string\""

       detector.detect(line, line.indexOf("+") + 2) must_== CompletionType.Scope
    }

    "detect scope completion after if in case statement" in {
      val line = "  case Some(list) if  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope completion in between if in case statement and =>" in {
      val line = "  case Some(list) if   =>"

      detector.detect(line, line.indexOf("if") + 2) must_== CompletionType.Scope

    }

    "detect type completion in import" in {
      val line = "import scalaz. "

      detector.detect(line, line.length - 1) must_== CompletionType.Type
    }

    "detect scope completion in empty import" in {
      val line = "import  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect type completion in import with curly braces" in {
      val line = "     import scalaz.{Monad, }"

      detector.detect(line, line.length - 2) must_== CompletionType.Type
    }

    "detect scope completion after $ inside interpolated string" in {
      val line = "s\"some text $ \""

      detector.detect(line, line.indexOf("$") + 1) must_== CompletionType.Scope
    }

    "detect scope completion after ${ inside interpolated string" in {
      val line = "s\"some text ${\""

      detector.detect(line, line.indexOf("{") + 1) must_== CompletionType.Scope
    }

    "detect type completion in method call" in {
      val line = "add(list.)"

      detector.detect(line, line.indexOf('.') + 1) must_== CompletionType.Type
    }

    "detect type completion after parametrized method" in {
      val line = "val future = (facade ? completeAt()).mapTo[CompletionResult[String]]. "

      detector.detect(line, line.length - 1) must_== CompletionType.Type
    }

    "detect type completion with out of bound position" in {
      val line = "    case FileSystemEvents."

      detector.detect(line, line.length) must_== CompletionType.Type
    }
  }
}
