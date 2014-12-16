package vim.scalacompletion.imports

import java.nio.file.Path
import vim.scalacompletion.compiler.{CompilerModule, Compiler}

import scala.reflect.internal.util.SourceFile
import scalaz._
import Scalaz._

trait ImportsIndexModule { self: ClassFileIndexModule with SourceIndexModule with CompilerModule =>
  def createImportsIndex(classpath: Set[Path], sources: Seq[SourceFile]): Reader[Compiler, ImportsIndex] =
    Reader { compiler =>
      val classFileIndex = buildIndexFromClassFiles(classpath).run
      val sourcesIndex = createIndexForSources(sources).run(compiler)
      ImportsIndex(sourcesIndex, classFileIndex)
    }
}
