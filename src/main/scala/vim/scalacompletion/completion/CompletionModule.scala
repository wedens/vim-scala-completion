package vim.scalacompletion.completion

import vim.scalacompletion.{PositionInSource, SourcesManagementModule}
import vim.scalacompletion.compiler.{CompilerModule, Compiler}

import scala.reflect.internal.util.{Position, SourceFile}
import scalaz._
import Scalaz._

trait CompletionModule { self: SourcesManagementModule with CompilerModule with CompletionTypeDetectionModule with MemberInfoModule =>
  lazy val completion = new Completion

  class Completion {
    private def positionInSource(source: SourceFile, lineIdx: Int, columnIdx: Int): Position = {
      val lineOffset = source.lineToOffset(lineIdx)
      source.position(lineOffset + columnIdx)
    }

    private def detectCompletionType(position: Position): CompletionType = {
      val lineContent = position.source.lineToString(position.line)
      completionTypeDetector.detectAt(lineContent, position.column - 1) // TODO: why -1
    }

    def members(completionType: CompletionType, position: Position): Reader[Compiler, Seq[MemberInfo]] =
      Reader { compiler =>
        val memberInfoExtractor = memberInfoFrom(compiler) _
        completionType match {
          case Scope => compiler.typeCompletion(position, memberInfoExtractor)
          case Type =>
            val lastCharPosition = position.point - 1
            compiler.scopeCompletion(position.withPoint(lastCharPosition), memberInfoExtractor)
          case NoCompletion => Seq.empty
        }
      }

  def complete(position: PositionInSource, prefix: Option[String]): Reader[Compiler, Seq[MemberInfo]] =
    for {
      source <- sourcesManagement.loadSource(position.sourceName, position.sourcePath)
      completionPosition = positionInSource(source, position.lineIdx, position.columnIdx)
      completionType = detectCompletionType(completionPosition)
      members <- members(completionType, completionPosition)
    } yield {
      members filter filterMember(prefix) sorted orderByRankDesc(prefix)
    }
  }
}
