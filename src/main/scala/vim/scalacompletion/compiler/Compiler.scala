package vim.scalacompletion.compiler

import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.reporters.StoreReporter
import java.io.File
import java.nio.file.Path

object Compiler {
  def apply(classpath: Set[Path]): Compiler = {
    val settings = new Settings()
    settings.classpath.value = classpath.mkString("", File.pathSeparator, "")

    val reporter = new StoreReporter()

    new Compiler(settings, reporter)
  }
}

class Compiler(settings: Settings, _reporter: Reporter, projectName: String = "") extends Global(settings, _reporter, projectName) with CompilerApi
