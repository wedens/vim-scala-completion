package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import org.specs2.specification.Scope
import org.specs2.matcher.ThrownExpectations
import scala.reflect.internal.util.Position
import org.mockito.Matchers.{eq => meq}

trait completion extends Scope with Mockito with ThrownExpectations {
  val completionTypeDetector = mock[CompletionTypeDetector]
  val compiler = mock[Compiler]
  val extractor = mock[MemberInfoExtractor[String]]
  val filter = mock[MemberFilter[String]]
  val memberRankCalculator = mock[MemberRankCalculator[String]]
  val handler = new CompletionHandler(completionTypeDetector,
                                      compiler, extractor, filter,
                                      memberRankCalculator)

  val position = mock[Position]
  filter.apply(any, any) returns true
}

trait scopeCompletion extends completion {
  val scopeCompletionResult = Seq("String", "Option", "Seq")
  completionTypeDetector.detect(position) returns CompletionType.Scope
  compiler.scopeCompletion[String](position, extractor) returns scopeCompletionResult
}

trait typeCompletion extends completion {
  val typeCompletionResult = Seq("toString", "map", "foldLeft")
  completionTypeDetector.detect(position) returns CompletionType.Type
  compiler.typeCompletion[String](position, extractor) returns typeCompletionResult
}

class CompletionHandlerSpec extends Specification with Mockito {

  "completion handler" should {
    "return type completion result" in new typeCompletion {
      handler.complete(position) must_== typeCompletionResult
    }

    "return scope completion result" in new scopeCompletion {
      handler.complete(position) must_== scopeCompletionResult
    }

    "return empty list when no completion" in new completion {
      completionTypeDetector.detect(position) returns CompletionType.NoCompletion

      handler.complete(position) must be empty
    }

    "filter completion result" in new typeCompletion {
      filter.apply(any, any) returns true
      filter.apply(any, meq("toString")) returns false

      handler.complete(position) must_== Seq("map", "foldLeft")
    }

    "sort members by rank" in new typeCompletion {
      memberRankCalculator.apply(any, any) returns 0
      memberRankCalculator.apply(any, meq("foldLeft")) returns 10

      handler.complete(position) must_== Seq("foldLeft", "toString", "map")
    }

    "limit result by 1" in new typeCompletion {
      handler.complete(position, Some(1)) must have size(1)
    }

    "decrease position by 1 for type completion" in pending
  }
}
