package vim.scalacompletion

import org.specs2.mutable._

class CompletionTypeDetectorSpec extends Specification {
  val detector = new CompletionTypeDetector

  "completion type detector" should {
    "detect type completion after ." in {
      val line = "str."

      detector.detect(line, line.indexOf(".")) must_== CompletionType.Type
    }

    "detect scope completion on empty line" in  {
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

    "detect type completion after word with preceeding spaces" in {
      val line = "   str  "

      detector.detect(line, line.length - 1) must_== CompletionType.Type
    }

    "detect scope completion after infix method call" in {
      val line = "someVar |@|  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope completion after infix method call with preceeding colon" in {
      val line = "f(); actor !  "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope completion iside of []" in {
      val line = "type X[T] = Either[T, ]"

      detector.detect(line, line.length - 3) must_== CompletionType.Scope
    }

    "detect scope completion inside function arguments" in {
      val line = "list.map( )"

      detector.detect(line, line.indexOf(" ")) must_== CompletionType.Scope
    }

    "detect type completion in the middle of expression" in {
       val line = "f(). match"

       detector.detect(line, line.indexOf(".")) must_== CompletionType.Type
    }

    "detect type completion in the middle of expression with infix operator" in {
       val line = "val list = x   y :: z :: Nil"

       detector.detect(line, line.indexOf("x") + 3) must_== CompletionType.Type
    }

    "detect scope completion in the middle of type expression" in {
      val line = "type X[T] = ValidationNel[ ,Option[T]]"

      detector.detect(line, line.indexOf(",") - 1) must_== CompletionType.Scope
    }

    "detect scope in the middle of case expression" in {
      val line = "  case Some(value) if value ==   => value"

      detector.detect(line, line.indexOf("=>") - 1) must_== CompletionType.Scope
    }

    "detect scope after case statement" in {
      val line = "  case   =>"

      detector.detect(line, line.indexOf("=>") - 1) must_== CompletionType.Scope
    }

    "detect scope after new statement" in {
      val line = "val x = new "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after extends statement" in {
      val line = "class MyClass extends "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after with statement" in {
      val line = "class MyClass extends Base with "

      detector.detect(line, line.length - 1) must_== CompletionType.Scope
    }

    "detect scope after yield statement" in {
      val line = "for (x <- y) yield "

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

    "detect scope completion after $ inside interpolated string" in {
      val line = "s\"some text $ \""

      detector.detect(line, line.indexOf("$")) must_== CompletionType.Scope
    }
  }
}
