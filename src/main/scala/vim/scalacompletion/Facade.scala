package vim.scalacompletion

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.interactive.Global

trait Facade {
  type MemberInfoType

  val compilerApi: Global with CompilerApi
  val completionTypeDetector: CompletionTypeDetector
  val extractor: compilerApi.Member => MemberInfoType

  def completeAt(body: String, offset: Int, column: Int): Seq[MemberInfoType] = {
    val source = new BatchSourceFile("test", body)
    compilerApi.addSources(List(source))
    val lineIdx = source.offsetToLine(offset)
    val sourceLine = source.lineToString(lineIdx)
    val position = source.position(offset)
    completionTypeDetector.detect(sourceLine, column) match {
      case CompletionType.Type => compilerApi.typeCompletion(position, extractor)
      case CompletionType.Scope => compilerApi.scopeCompletion(position, extractor)
      case _ => Seq.empty
    }
  }
}
