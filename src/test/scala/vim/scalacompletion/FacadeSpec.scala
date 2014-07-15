package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import scala.tools.nsc.interactive.Global

trait Api extends Global with CompilerApi

class FacadeSpec extends Specification with Mockito with Before { self =>
  val compilerApi: Global with CompilerApi = mock[Api]
  val completionTypeDetector: CompletionTypeDetector = mock[CompletionTypeDetector]
  val extractor: compilerApi.Member => String = m => m.toString

  val facade = new Facade {
    type MemberInfoExtractor = String
    val compilerApi = self.compilerApi
    val completionTypeDetector = self.completionTypeDetector
    val extractor: compilerApi.Member => MemberInfoType = m => m.toString
  }

  def before = println("")
}
