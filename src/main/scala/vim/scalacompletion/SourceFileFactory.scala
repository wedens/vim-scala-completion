package vim.scalacompletion

import scala.reflect.internal.util.SourceFile
import scala.reflect.internal.util.BatchSourceFile
import java.nio.file.{Files, Paths}

trait SourceFileFactory {
  def createSourceFile(name: String, path: String): SourceFile
}

class SourceFileFactoryImpl extends SourceFileFactory {
  def createSourceFile(name: String, path: String) = {
    new BatchSourceFile(path, fileContent(path))
  }

  private def fileContent(path: String) = {
    new String(Files.readAllBytes(Paths.get(path))).toCharArray
  }
}
