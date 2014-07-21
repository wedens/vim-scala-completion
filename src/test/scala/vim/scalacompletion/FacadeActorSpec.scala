package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import org.specs2.time.NoTimeConversions
import scala.tools.nsc.interactive.Global
import scala.reflect.internal.util.SourceFile
import scala.concurrent.duration._
import org.specs2.specification.BeforeExample
import org.mockito.Matchers.{eq => meq}
import java.io.{File => JFile}
import akka.actor._
import akka.pattern.ask
import akka.testkit.{ TestActorRef, TestKit }
import akka.util.Timeout
import scala.util.{Try, Success, Failure}
import FacadeActor._


class FacadeActorSpec extends TestKit(ActorSystem("FacadeSpec"))
                      with SpecificationLike
                      with Mockito
                      with BeforeExample
                      with NoTimeConversions { selfSpec =>

  var compilerApi: Compiler = _
  var completionTypeDetector: CompletionTypeDetector = _
  var sourceFileFactory: SourceFileFactory = _
  var membersFilter: MemberFilter[String] = _
  var memberRankCalculator: MemberRankCalculator[String] = _
  var scalaSourcesFinder: ScalaSourcesFinder = _

  var facade: TestActorRef[FacadeActor[String]] = _

  implicit val timeout = Timeout(5 seconds)
  val sourceName = "/src/main/scala/pkg/Source.scala"
  val sourcePath = "/tmp/6157147744291722932"
  val offset = 35
  val column = 15

  def before = {
    compilerApi = mock[Compiler]
    compilerApi.typeCompletion(any, any) returns List()
    compilerApi.scopeCompletion(any, any) returns List()

    completionTypeDetector = mock[CompletionTypeDetector]
    sourceFileFactory = mock[SourceFileFactory]
    scalaSourcesFinder = mock[ScalaSourcesFinder]

    membersFilter = mock[MemberFilter[String]]
    membersFilter.apply(any, any) returns true

    memberRankCalculator = mock[MemberRankCalculator[String]]
    memberRankCalculator.apply(any, any) returns 0

    facade = TestActorRef(new FacadeActor[String] {
      val compilerApi = selfSpec.compilerApi
      val completionTypeDetector = selfSpec.completionTypeDetector
      val extractor: compilerApi.Member => String = m => m.toString
      val sourceFileFactory = selfSpec.sourceFileFactory
      val membersFilter = selfSpec.membersFilter
      val memberRankCalculator = selfSpec.memberRankCalculator
      val scalaSourcesFinder = selfSpec.scalaSourcesFinder
    })
  }

  sequential

  "facade" should {
    "completion" should {
      "update source" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(compilerApi).addSources(any[List[SourceFile]])
      }

      "detect completion type" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(completionTypeDetector).detect(anyString, anyInt)
      }

      "call type completion when detector says type" in  {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(compilerApi).typeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "call scope completion when detector says scope" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Scope

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(compilerApi).scopeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "not call any completion when detector says no completion" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(compilerApi).addSources(any)
        there were noMoreCallsTo(compilerApi)
      }

      "return empty seq when no completion" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        val future = facade ? CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        val Success(result: CompletionResult[String]) = future.mapTo[CompletionResult[String]].value.get
        result.members must be empty
      }

      "call completion type detector with correct parameters" in {
        stubSourceFactory(line = "abc123")

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(completionTypeDetector).detect("abc123", 15)
      }

      "create source with correct parameters" in {
        stubSourceFactory()

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(sourceFileFactory).createSourceFile(sourceName, sourcePath)
      }

      "get position equal to offset for scope completion" in {
        val source = stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Scope

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(source).position(offset)
      }

      "get position with offset before dot or space for type completion" in {
        val source = stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(source).position(offset - 1)
      }

      "filter members" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some("pfx"))

        there was two(membersFilter).apply(Some("pfx"), "str")
      }

      "rank members" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(memberRankCalculator).apply(any, meq("str"))
      }

      "rank members with prefix" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some("pfx"))

        there was one(memberRankCalculator).apply(meq(Some("pfx")), any)
      }

      "sort members by rank desc" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str", "str2")
        memberRankCalculator.apply(any, anyString) returns 1 thenReturns 10

        val future = facade ? CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        val Success(result: CompletionResult[String]) = future.mapTo[CompletionResult[String]].value.get
        result.members must_== Seq("str2", "str")
      }

      "limit result by 15" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns (1 to 15).map(_.toString)

        val future = facade ? CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        val Success(result: CompletionResult[String]) = future.mapTo[CompletionResult[String]].value.get
        result.members must have size(15)
      }
    }

    "reloading all sources in directories" should {
      val file1Mock = mock[JFile]
      file1Mock.getCanonicalPath returns "/tmp/file1.scala"
      val file2Mock = mock[JFile]
      file2Mock.getCanonicalPath returns "/opt/file2.scala"
      val dirs = List("/tmp", "/opt")
      val files = Seq(file1Mock, file2Mock)

      "find sources in directories" in {
        scalaSourcesFinder.findIn(any) returns files

        facade ! ReloadSourcesInDirs(dirs)

        there was one(scalaSourcesFinder).findIn(List(new JFile("/tmp"), new JFile("/opt")))
      }

      "create compiler's source files for found sources" in {
        scalaSourcesFinder.findIn(any) returns files

        facade ! ReloadSourcesInDirs(dirs)

        there was one(sourceFileFactory).createSourceFile("/tmp/file1.scala") andThen one(sourceFileFactory).createSourceFile("/opt/file2.scala")
      }

      "ask compiler to reload sources" in {
        scalaSourcesFinder.findIn(any) returns files

        facade ! ReloadSourcesInDirs(dirs)

        there was one(compilerApi).addSources(any)
      }
    }
  }

  def stubSourceFactory(lineIdx: Int = 0, line: String = "") = {
    val mockSourceFile = mock[SourceFile]
    mockSourceFile.lineToString(anyInt) returns line
    mockSourceFile.offsetToLine(anyInt) returns lineIdx
    mockSourceFile.position(anyInt) returns mock[scala.reflect.internal.util.Position]
    sourceFileFactory.createSourceFile(anyString, anyString) returns mockSourceFile
    mockSourceFile
  }
}
