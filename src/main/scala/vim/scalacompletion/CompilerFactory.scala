package vim.scalacompletion

import java.io.{File => JFile}
import scala.tools.nsc.reporters.{StoreReporter, ConsoleReporter}
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global

object CompilerFactory {
  def apply(jars: Seq[JFile]) = {
    val settings = new Settings()
    val sep = JFile.pathSeparator
    settings.classpath.value = jars.map(_.getAbsolutePath).mkString("", sep, "")

    val reporter = new ConsoleReporter(settings)
    new Compiler(settings, reporter)
  }
}
