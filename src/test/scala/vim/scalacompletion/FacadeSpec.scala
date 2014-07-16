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

  var facade: Facade = _

  val body = """package test
  class X[T <: A[_]](v: T) {

  }
  """

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

  sequential

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

      "call type completion when detector says type" in  {
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type

        facade.completeAt(body, 35, 15)

        there was one(compilerApi).typeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "call scope completion when detector says scope" in {
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Scope

        facade.completeAt(body, 35, 15)

        there was one(compilerApi).scopeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "not call any completion when detector says no completion" in {
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(body, 35, 15)

        there was one(compilerApi).addSources(any)
        there were noMoreCallsTo(compilerApi)
      }

      "return empty seq when no completion" in {
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade.completeAt(body, 35, 15) must be empty
      }

      "call completion type detector with correct parameters" in {
        facade.completeAt(body, 35, 15)

        there was one(completionTypeDetector).detect(body.split('\n')(1), 15)
      }
    }
  }
}
