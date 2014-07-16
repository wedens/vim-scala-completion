package vim.scalacompletion

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.interactive.Response
import scala.reflect.internal.util.{SourceFile, Position}
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global

class Compiler(settings: Settings, _reporter: Reporter, projectName: String = "") extends Global(settings, _reporter, projectName) with CompilerApi

trait CompilerApi extends WithLog { self: Global =>

  def addSources(sources: List[SourceFile]) = {
    logg.debug(s"Reloading sources: ${sources.mkString(",")}")
    val reloadResult = new Response[Unit]
    askReload(sources, reloadResult)
    reloadResult.get
  }

  def typeCompletion[T](position: Position, extractor: Member => T): Seq[T] = {
    val completeResult = new Response[List[Member]]
    askTypeCompletion(position, completeResult)
    completeResult.get match {
      case Left(matches) =>
        ask { () =>
          matches map extractor
        }
      case Right(ex) => throw ex
    }
  }

  def scopeCompletion[T](position: Position, extractor: Member => T) = {
    val completeResult = new Response[List[Member]]
    askScopeCompletion(position, completeResult)
    completeResult.get match {
      case Left(matches) =>
        ask { () =>
          matches map extractor
        }
      case Right(ex) => throw ex
    }
  }

  def removeSources(sources: List[SourceFile]) = {
    logg.debug(s"Removing sources: ${sources.mkString(",")}")
    val removeResult = new Response[Unit]
    askFilesDeleted(sources, removeResult)
    removeResult.get
  }
}
