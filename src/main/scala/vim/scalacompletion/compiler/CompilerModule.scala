package vim.scalacompletion.compiler

import java.nio.file.Path

trait CompilerModule {
  def createCompiler(classpath: Set[Path]): Compiler = Compiler(classpath)
}
