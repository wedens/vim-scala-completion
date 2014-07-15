package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import scala.tools.nsc.interactive.Global
import scala.reflect.internal.util.SourceFile

trait Api extends Global with CompilerApi

class FacadeSpec extends Specification with Mockito with Before { self =>
  var compilerApi: Compiler = _
  var completionTypeDetector: CompletionTypeDetector = _

  var facade: Facade = _

  def before = {
    compilerApi = mock[Compiler]
    completionTypeDetector = mock[CompletionTypeDetector]

    facade = new Facade {
      type MemberInfoType = String
      val compilerApi = self.compilerApi
      val completionTypeDetector = self.completionTypeDetector
      val extractor: compilerApi.Member => MemberInfoType = m => m.toString
    }
  }

  val body = """package test
  class X[T <: A[_]](v: T) {

  }
  """
  "facade" should {
    "completion" should {
      "update source" in {
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(body, 35, 15)

        there was one(compilerApi).addSources(any[List[SourceFile]])
      }

      "detect completion type" in {
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(body, 35, 15)

        there was one(completionTypeDetector).detect(anyString, anyInt)
      }

      "call type completion" in  {
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type

        facade.completeAt(body, 35, 15)

        there was one(compilerApi).typeCompletion(any[scala.reflect.internal.util.Position], any[])
      }
      "call scope completion" in pending
      "not call any completion" in pending
      "return empty seq when no completion" in pending
    }
  }
}
