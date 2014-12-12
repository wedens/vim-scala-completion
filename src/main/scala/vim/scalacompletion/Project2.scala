package vim.scalacompletion

import vim.scalacompletion.config.ConfigurationModule
import vim.scalacompletion.imports.ClassFileIndexModule
import vim.scalacompletion.compiler.Compiler
import java.nio.file.Path
import akka.actor.Actor

object Project2 {
  case class Complete(fileName: String, filePath: String,
    lineIdx: Int, columnIdx: String, prefix: Option[String])

  case class GetImportSuggestions(className: String)

  case class FindDeclaration(fileName: String, filePath: String,
    lineIdx: Int, columnIdx: String)

  case class CalculatePackage(filePath: String)
}

trait Project2 extends Actor {
  self: ConfigurationModule with JavaLibFinder with ClassFileIndexModule =>

  def configPath: Path

  import Project2._

  private val projectConfig = configLoader.parseConfig(configPath)
  private val classFileIndex = buildClassFileIndex(projectConfig.classpath + locateJavaLib.get)
  private val compiler = Compiler(projectConfig.classpath)

  def receive = {
    case Complete(fileName, filePath, lineIdx, columnIdx, prefix) =>
    case GetImportSuggestions(className) =>
    case FindDeclaration(fileName, filePath, lineIdx, columnIdx) =>
    case CalculatePackage(filePath) =>
  }
}

import java.nio.file.{Path, Paths}

trait JavaLibFinder {
  def locateJavaLib: Option[Path]
}

trait JavaLibFinderFromBootClasspath extends JavaLibFinder {
  lazy val locateJavaLib: Option[Path] = {
    System.getProperty("sun.boot.class.path")
      .split(":")
      .find(_.endsWith("rt.jar"))
      .map(Paths.get(_))
  }
}
