package vim.scalacompletion.compiler

import vim.scalacompletion.WithLog

import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.Reporter

class Compiler(settings: Settings, _reporter: Reporter, projectName: String = "") extends Global(settings, _reporter, projectName) with CompilerApi

trait CompilerApi extends WithLog { self: Global =>

  def reloadSources(sources: List[SourceFile]) = {
    logg.debug(s"Reloading sources: ${sources.mkString(",")}")
    withResponse[Unit](r => askReload(sources, r)).get
  }

  def typeCompletion[T](position: Position, extractor: Member => T): Seq[T] = {
    withResponse[List[Member]](r => askTypeCompletion(position, r)).get match {
      case Left(matches) =>
        ask { () =>
          matches map extractor
        }
      case Right(ex) =>
        logg.debug("Exception during type completion", ex)
        Seq.empty
    }
  }

  def scopeCompletion[T](position: Position, extractor: Member => T): Seq[T] = {
    withResponse[List[Member]](r => askScopeCompletion(position, r)).get match {
      case Left(matches) =>
        ask { () =>
          matches map extractor
        }
      case Right(ex) =>
        logg.debug("Exception during scope completion", ex)
        Seq.empty
    }
  }

  def removeSources(sources: List[SourceFile]) = {
    logg.debug(s"Removing sources: ${sources.mkString(",")}")
    withResponse[Unit](r => askFilesDeleted(sources, r)).get
  }

  private def withResponse[A](op: Response[A] => Any): Response[A] = {
    val response = new Response[A]
    op(response)
    response
  }
}
