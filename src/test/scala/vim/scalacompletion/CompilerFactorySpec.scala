package vim.scalacompletion

import org.specs2.mutable._
import java.io.{File => JFile}
import scala.tools.nsc.reporters.{StoreReporter, ConsoleReporter}

class CompilerFactorySpec extends Specification {
  val rtJarPath = "/rt.jar"
  val scalaLibJarPath = "/scala-library-2.11.1.jar"
  val scalazJarPath = "/scalaz-core_2.11-7.0.6.jar"

  def jars = Seq(
    new JFile(getClass().getResource(rtJarPath).toURI), 
    new JFile(getClass().getResource(scalaLibJarPath).toURI),
    new JFile(getClass().getResource(scalazJarPath).toURI)
  )

  "compiler factory" should {
    "have expected classpath" in {
      val settings = CompilerFactory(jars).settings

      settings.classpath.toString must contain(rtJarPath)
      settings.classpath.toString must contain(scalaLibJarPath)
      settings.classpath.toString must contain(scalazJarPath)
    }

    "have console reporter" in {
      val compiler = CompilerFactory(jars)

      compiler.reporter must beAnInstanceOf[ConsoleReporter]
    }
  }
}
