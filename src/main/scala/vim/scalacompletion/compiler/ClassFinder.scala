package vim.scalacompletion.compiler

import scala.tools.nsc.interactive.Global
import scala.reflect.internal.util.SourceFile
import vim.scalacompletion.WithLog
import vim.scalacompletion.imports.SourceFQCN

trait ClassFinder extends WithLog with CompilerHelpers { self: Global =>
  private class ClassTraverser extends Traverser {
    var fqcns = Seq.empty[SourceFQCN]

    override def traverse(tree: Tree): Unit = tree match {
      case cd @ ClassDef(_, _, _, _) =>
        val sym = tree.symbol
        val isClass = sym.isClass || sym.isTrait || sym.isModule
        if (isClass && sym.isPublic) {
          val scope = sym.owner.fullName
          val name = sym.name.toString
          val file = sym.pos.source.path
          val fqcn = SourceFQCN(scope, name, file)
          fqcns = fqcns :+ fqcn
        }
      case _ => super.traverse(tree)
    }
  }

  private def findClassesIn(tree: Tree): Seq[SourceFQCN] = {
    val t = new ClassTraverser
    t.traverse(tree)
    t.fqcns
  }

  // TODO: parallelize?
  def fqcnsFromSources(sources: List[SourceFile]): Set[SourceFQCN] = {
    sources.flatMap { source =>
      withResponse[Tree](r => askLoadedTyped(source, true, r)).get match {
        case Left(tree) => findClassesIn(tree)
        case Right(ex) =>
          logg.debug(s"Problem obtaining classes from ${source.path}", ex)
          Seq.empty
      }
    }.toSet
  }
}

