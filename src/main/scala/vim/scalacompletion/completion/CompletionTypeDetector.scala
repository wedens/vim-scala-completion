package vim.scalacompletion.completion

import scala.tools.nsc.interactive.Global
import vim.scalacompletion.compiler.CompilerApi
import CompletionType._
import CompletionContext._

class CompletionTypeDetector(compiler: Global, compilerApi: CompilerApi) {
  def detectAt(position: Global#Position): CompletionType  = {
    val tree = compilerApi.getTypeAt(position)

    tree match {
      case compiler.Import(_: compiler.Select, _) => Type(Some(ImportContext))
      case _: compiler.Import => Scope(Some(ImportContext))
      case _: compiler.Template => Scope(Some(TypeNameContext))
      case _: compiler.Select => Type(Some(MemberSelectionContext))
      case _: compiler.New => Scope(Some(NewContext))
      case compiler.Ident(_: compiler.TypeName) => Scope(Some(TypeNameContext))
      case compiler.Apply(_: compiler.Select, _) => Type(Some(MemberSelectionContext))
      case compiler.Apply(_: compiler.New, _) => Scope(Some(NewContext))
      // see test for comments on this comments
      // case compiler.Apply(_: compiler.TypeApply, _) => Scope(Some(TypeNameContext))
      // case _: compiler.TypeApply => Scope(Some(TypeNameContext))
      case _ => Scope()
    }
  }
}
