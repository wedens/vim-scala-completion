package vim.scalacompletion.compiler

import scala.tools.nsc.interactive.Global
import vim.scalacompletion.imports.SourceFQCN

import scalaz.Reader

trait FqcnsCollectorFromTree {
  def createFqcnsFromTreeCollector(global: Global)(tree: global.Tree): Set[SourceFQCN] = {
    import global._
    var fqcns = Set.empty[SourceFQCN]
    val traverser = new Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case cd @ ClassDef(_, _, _, _) =>
          val sym = tree.symbol
          val isClass = sym.isClass || sym.isTrait || sym.isModule
          if (isClass && sym.isPublic) {
            val scope = sym.owner.fullName
            val name = sym.name.toString
            val file = sym.pos.source.path
            val fqcn = SourceFQCN(scope, name, file)
            fqcns = fqcns + fqcn
          }
        case _ => super.traverse(tree)
      }
    }
    traverser.traverse(tree)
    fqcns
  }
}
