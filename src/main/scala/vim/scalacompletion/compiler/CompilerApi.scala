package vim.scalacompletion.compiler

import vim.scalacompletion.WithLog
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.interactive.Global

trait CompilerApi extends WithLog { self: Global =>

  def reloadSources(sources: List[SourceFile]) = {
    logg.debug(s"Reloading sources: ${sources.mkString(",")}")
    try {
      withResponse[Unit](r => askReload(sources, r)).get
    } catch {
      case ex: Throwable => logg.debug("Exception while removing sources", ex)
    }
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
    try {
      withResponse[Unit](r => askFilesDeleted(sources, r)).get
    } catch {
      case ex: Throwable => logg.debug("Exception while removing sources", ex)
    }
  }

  def getTypeAt(position: Global#Position): Global#Tree = {
    withResponse[Tree](r => askTypeAt(position, r)).get.left.get
  }

  private def withResponse[A](op: Response[A] => Any): Response[A] = {
    val response = new Response[A]
    op(response)
    response
  }
}
