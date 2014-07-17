package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import scala.tools.nsc.interactive.Global
import scala.reflect.internal.util.SourceFile
import org.specs2.specification.BeforeExample
import org.mockito.Matchers.{eq => meq}

trait Api extends Global with CompilerApi

class FacadeSpec extends Specification with Mockito with BeforeExample { self =>
  var compilerApi: Compiler = _
  var completionTypeDetector: CompletionTypeDetector = _
  var sourceFileFactory: SourceFileFactory = _
  var membersFilter: String => Boolean = _
  var memberRankCalculator: MemberRankCalculator[String] = _

  var facade: Facade = _

  val sourceName = "/src/main/scala/pkg/Source.scala"
  val sourcePath = "/tmp/6157147744291722932"

  def before = {
    compilerApi = mock[Compiler]
    compilerApi.typeCompletion(any, any) returns List()
    compilerApi.scopeCompletion(any, any) returns List()

    completionTypeDetector = mock[CompletionTypeDetector]
    sourceFileFactory = mock[SourceFileFactory]

    membersFilter = mock[Function1[String, Boolean]]
    membersFilter.apply(any) returns true

    memberRankCalculator = mock[MemberRankCalculator[String]]
    memberRankCalculator.apply(any, any) returns 0

    facade = new Facade {
      type MemberInfoType = String
      val compilerApi = self.compilerApi
      val completionTypeDetector = self.completionTypeDetector
      val extractor: compilerApi.Member => MemberInfoType = m => m.toString
      val sourceFileFactory = self.sourceFileFactory
      val membersFilter = self.membersFilter
      val memberRankCalculator = self.memberRankCalculator
    }
  }

  sequential

  "facade" should {
    "completion" should {
      "update source" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(compilerApi).addSources(any[List[SourceFile]])
      }

      "detect completion type" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(completionTypeDetector).detect(anyString, anyInt)
      }

      "call type completion when detector says type" in  {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(compilerApi).typeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "call scope completion when detector says scope" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Scope

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(compilerApi).scopeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "not call any completion when detector says no completion" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(compilerApi).addSources(any)
        there were noMoreCallsTo(compilerApi)
      }

      "return empty seq when no completion" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(sourceName, sourcePath, 35, 15, Some("")) must be empty
      }

      "call completion type detector with correct parameters" in {
        stubSourceFactory(line = "abc123")

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(completionTypeDetector).detect("abc123", 15)
      }

      "create source with correct parameters" in {
        stubSourceFactory()

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(sourceFileFactory).createSourceFile(sourceName, sourcePath)
      }

      "filter members" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(membersFilter).apply("str")
      }

      "rank members" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade.completeAt(sourceName, sourcePath, 35, 15, Some(""))

        there was one(memberRankCalculator).apply(any, meq("str"))
      }

      "rank members with prefix" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade.completeAt(sourceName, sourcePath, 35, 15, Some("pfx"))

        there was one(memberRankCalculator).apply(meq(Some("pfx")), any)
      }

      "sort members by rank desc" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str", "str2")
        memberRankCalculator.apply(any, anyString) returns 1 thenReturns 10

        facade.completeAt(sourceName, sourcePath, 35, 15, Some("")) must_== Seq("str2", "str")
      }

      "limit result by 10" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns (1 to 15).map(_.toString)

        facade.completeAt(sourceName, sourcePath, 35, 15, Some("")) must have size(10)
      }
    }
  }

  def stubSourceFactory(lineIdx: Int = 0, line: String = "") = {
    val mockSourceFile = mock[SourceFile]
    mockSourceFile.lineToString(anyInt) returns line
    mockSourceFile.offsetToLine(anyInt) returns lineIdx
    mockSourceFile.position(anyInt) returns mock[scala.reflect.internal.util.Position]
    sourceFileFactory.createSourceFile(anyString, anyString) returns mockSourceFile
  }
}
