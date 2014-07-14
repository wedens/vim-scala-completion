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

    // "detect type completion in the middle of expression with infix operator" in {
    //    val line = "x   y :: z :: Nil"

    //    detector.detect(line, line.indexOf("x") + 2) must_== CompletionType.Type
    // }

    "detect scope completion in the middle of type expression" in {
      val line = "type X[T] = ValidationNel[ ,Option[T]]"

      detector.detect(line, line.indexOf(",") - 1) must_== CompletionType.Scope
    }

    "detect scope in the middle of case expression" in {
      val line = "  case Some(value) if value ==   => value"

      detector.detect(line, line.indexOf("=>") - 1) must_== CompletionType.Scope
    }
  }
}
