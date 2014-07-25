package vim.scalacompletion.compiler

import java.io.{File => JFile}
import vim.scalacompletion.WithLog
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter

trait CompilerFactory {
  def create(jars: Seq[JFile]): Compiler
}

class CompilerFactoryImpl extends CompilerFactory with WithLog {
  def create(jars: Seq[JFile]) = {
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
