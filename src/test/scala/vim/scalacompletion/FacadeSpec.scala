package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import scala.tools.nsc.interactive.Global
import scala.reflect.internal.util.SourceFile
import org.specs2.specification.BeforeExample

trait Api extends Global with CompilerApi

class FacadeSpec extends Specification with Mockito with BeforeExample { self =>
  var compilerApi: Compiler = _
  var completionTypeDetector: CompletionTypeDetector = _
  var sourceFileFactory: SourceFileFactory = _

  var facade: Facade = _

  val sourceName = "/src/main/scala/pkg/Source.scala"
  val sourcePath = "/tmp/6157147744291722932"

  def before = {
    compilerApi = mock[Compiler]
    completionTypeDetector = mock[CompletionTypeDetector]
    sourceFileFactory = mock[SourceFileFactory]

    facade = new Facade {
      type MemberInfoType = String
      val compilerApi = self.compilerApi
      val completionTypeDetector = self.completionTypeDetector
      val extractor: compilerApi.Member => MemberInfoType = m => m.toString
      val sourceFileFactory = self.sourceFileFactory
    }
  }

  sequential

  "facade" should {
    "completion" should {
      "update source" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(sourceName, sourcePath, 35, 15)

        there was one(compilerApi).addSources(any[List[SourceFile]])
      }

      "detect completion type" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(sourceName, sourcePath, 35, 15)

        there was one(completionTypeDetector).detect(anyString, anyInt)
      }

      "call type completion when detector says type" in  {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type

        facade.completeAt(sourceName, sourcePath, 35, 15)

        there was one(compilerApi).typeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "call scope completion when detector says scope" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Scope

        facade.completeAt(sourceName, sourcePath, 35, 15)

        there was one(compilerApi).scopeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "not call any completion when detector says no completion" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(sourceName, sourcePath, 35, 15)

        there was one(compilerApi).addSources(any)
        there were noMoreCallsTo(compilerApi)
      }

      "return empty seq when no completion" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(sourceName, sourcePath, 35, 15) must be empty
      }

      "call completion type detector with correct parameters" in {
        stubSourceFactory(line = "abc123")

        facade.completeAt(sourceName, sourcePath, 35, 15)

        there was one(completionTypeDetector).detect("abc123", 15)
      }

      "create source with correct parameters" in {
        stubSourceFactory()

        facade.completeAt(sourceName, sourcePath, 35, 15)

        there was one(sourceFileFactory).createSourceFile(sourceName, sourcePath)
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
