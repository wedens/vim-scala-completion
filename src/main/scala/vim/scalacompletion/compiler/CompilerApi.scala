package vim.scalacompletion.compiler

import vim.scalacompletion.WithLog

import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.interactive.Global

trait DeclarationFinder extends CompilerHelpers { self: Global =>
  def findDeclarationOfSymbolAt(pos: Position): Option[Position] = {
    withResponse[Tree](r => askTypeAt(pos, r)).get match {
      case Left(tree) => Some(tree.symbol.pos)
      case Right(ex) => None
    }
  }
}

trait CompilerApi extends WithLog with CompilerHelpers { self: Global =>
  def reloadSources(sources: List[SourceFile]): Unit = {
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

  def removeSources(sources: List[SourceFile]): Unit = {
    logg.debug(s"Removing sources: ${sources.mkString(",")}")
    withResponse[Unit](r => askFilesDeleted(sources, r)).get
  }
}

trait CompilerHelpers { self: Global =>
  def withResponse[A](op: Response[A] => Any): Response[A] = {
    val response = new Response[A]
    op(response)
    response
  }
}

