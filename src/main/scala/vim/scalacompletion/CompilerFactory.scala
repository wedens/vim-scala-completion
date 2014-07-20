package vim.scalacompletion

import java.io.{File => JFile}
import scala.tools.nsc.reporters.{StoreReporter, ConsoleReporter}
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global

object CompilerFactory extends WithLog {
  def apply(jars: Seq[JFile]) = {
    logg.info(s"Creating compiler...")
    val settings = new Settings()
    val sep = JFile.pathSeparator
    settings.classpath.value = jars.map(_.getAbsolutePath).mkString("", sep, "")

    val reporter = new StoreReporter()
    // val reporter = new ConsoleReporter(settings)
    val compiler = new Compiler(settings, reporter)
    if (logg.isDebugEnabled)
      logg.debug(s"Created compiler with classpath: ${settings.classpath.value}")
    else
      logg.info("Compiler created")
    compiler
  }
}
