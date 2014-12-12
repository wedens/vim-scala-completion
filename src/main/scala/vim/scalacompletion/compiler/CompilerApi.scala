package vim.scalacompletion.compiler

import vim.scalacompletion.WithLog

import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.interactive.Global

trait CompilerApi
  extends Completion
  with SourceManagement
  with TypeInformation
  with FqcnCollectorFromTree
  with DeclarationFinder { self: Global => }

trait TypeInformation extends CompilerHelpers { self: Global =>
  def typedTreeForSource(source: SourceFile): Tree = {
    withResponse[Tree](r => askLoadedTyped(source, true, r)).get match {
      case Left(tree) => tree
      case Right(ex) => throw ex
    }
  }
}

trait DeclarationFinder extends CompilerHelpers { self: Global =>
  def findDeclarationOfSymbolAt(pos: Position): Option[Position] = {
    withResponse[Tree](r => askTypeAt(pos, r)).get match {
      case Left(tree) => Some(tree.symbol.pos)
      case Right(ex) => None
    }
  }
}

trait Completion extends CompilerHelpers { self: Global =>
  def typeCompletion[T](position: Position, extractor: Member => T): Seq[T] = {
    withResponse[List[Member]](r => askTypeCompletion(position, r)).get match {
      case Left(matches) =>
        ask { () =>
          matches map extractor
        }
      case Right(ex) => Seq.empty
    }
  }

  def scopeCompletion[T](position: Position, extractor: Member => T): Seq[T] = {
    withResponse[List[Member]](r => askScopeCompletion(position, r)).get match {
      case Left(matches) =>
        ask { () =>
          matches map extractor
        }
      case Right(ex) => Seq.empty
    }
  }
}

trait SourceManagement extends CompilerHelpers { self: Global =>
  def reloadSources(sources: List[SourceFile]): Unit = {
    withResponse[Unit](r => askReload(sources, r)).get
  }

  def removeSources(sources: List[SourceFile]): Unit = {
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

