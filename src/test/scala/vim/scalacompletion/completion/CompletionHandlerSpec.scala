package vim.scalacompletion.completion

import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.ThrownExpectations
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope
import vim.scalacompletion.PositionFactory
import vim.scalacompletion.compiler.{MemberInfoExtractor, Compiler}

import scala.reflect.internal.util.{Position, SourceFile}

trait completion extends Scope with Mockito with ThrownExpectations {
  val completionTypeDetector = mock[CompletionTypeDetector]
  val compiler = mock[Compiler]
  val extractor = mock[MemberInfoExtractor[String]]
  val filter = mock[MemberFilter[String]]
  val memberRankCalculator = mock[MemberRankCalculator[String]]
  val positionFactory = mock[PositionFactory]
  val handler = new CompletionHandler(completionTypeDetector,
                                      compiler, extractor, filter,
                                      memberRankCalculator,
                                      positionFactory)

  val position = mock[Position]
  filter.apply(any, any) returns true
}

trait scopeCompletion extends completion {
  val scopeCompletionResult = Seq("String", "Option", "Seq")
  completionTypeDetector.detectAt(position) returns CompletionType.Scope()
  compiler.scopeCompletion[String](position, extractor) returns scopeCompletionResult
}

trait typeCompletion extends completion {
  val typeCompletionResult = Seq("toString", "map", "foldLeft", "foldRight")
  val offset = 1
  val source = mock[SourceFile]
  val typeCompletionPosition = mock[Position]
  position.source returns source
  position.point returns offset
  completionTypeDetector.detectAt(position) returns CompletionType.Type()
  positionFactory.create(source, offset - 1) returns typeCompletionPosition
  compiler.typeCompletion[String](typeCompletionPosition, extractor) returns typeCompletionResult
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
      completionTypeDetector.detectAt(position) returns CompletionType.NoCompletion

      handler.complete(position) must be empty
    }

    "filter completion result without prefix" in new typeCompletion {
      filter.apply(meq(None), any) returns true
      filter.apply(None, "toString") returns false

      handler.complete(position) must_== typeCompletionResult.filter(_ != "toString")
    }

    "filter completion result with respect to prefix" in new typeCompletion {
      val prefix = Some("fold")
      val expectedResult = Seq("foldLeft", "foldRight")
      filter.apply(meq(prefix), any) returns false
      filter.apply(prefix, "foldLeft") returns true
      filter.apply(prefix, "foldRight") returns true

      handler.complete(position, prefix) must_== expectedResult
    }

    "sort members by rank without prefix" in new typeCompletion {
      val highestRankMember = "foldLeft"
      val expectedResult = Seq(highestRankMember) ++
                           typeCompletionResult.filter(_ != highestRankMember)
      memberRankCalculator.apply(any, any) returns 0
      memberRankCalculator.apply(any, meq(highestRankMember)) returns 100

      handler.complete(position) must_== expectedResult
    }

    "sort members by rank with respect to prefix" in new typeCompletion {
      val prefix = Some("fold")
      val highestRankMembers = Seq("foldLeft", "foldRight")
      val expectedResult = highestRankMembers ++
                            typeCompletionResult.filterNot(highestRankMembers.contains(_))
      filter.apply(any, any) returns true
      memberRankCalculator.apply(meq(prefix), any) returns 0
      memberRankCalculator.apply(prefix, highestRankMembers(0)) returns 100
      memberRankCalculator.apply(prefix, highestRankMembers(1)) returns 99

      handler.complete(position, prefix) must_== expectedResult
    }

    "limit result by 1" in new typeCompletion {
      handler.complete(position, maxResults = Some(1)) must have size 1
    }

    "decrease position by 1 for type completion" in new typeCompletion {
      handler.complete(position)

      there was one(positionFactory).create(source, offset - 1)
    }
  }
}
