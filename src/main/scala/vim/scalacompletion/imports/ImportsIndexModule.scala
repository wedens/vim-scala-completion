package vim.scalacompletion.imports

import java.nio.file.Path
import vim.scalacompletion.compiler.{CompilerModule, Compiler}

import scala.reflect.internal.util.SourceFile
import scalaz._
import Scalaz._

trait ImportsIndexModule { self: ClassFileIndexModule with SourceIndexModule with CompilerModule =>
  def createImportsIndex(compiler: Compiler, classpath: Set[Path], sources: Seq[SourceFile]): ImportsIndex = {
    val sourcesIndex = createIndexForSources(compiler, sources)
    val classFileIndex = buildIndexFromClassFiles(classpath).run
    ImportsIndex(sourcesIndex, classFileIndex)
  }

  def removeSourceFromIndex(importsIndex: ImportsIndex, source: SourceFile): ImportsIndex = {
    val sourceIndex = importsIndex.sourceIndex
    val updatedSourcesIndex = updateIndexForRemovedSources(sourceIndex, source :: Nil)
    importsIndex.copy(sourceIndex = updatedSourcesIndex)
  }

  def updateSourceInIndex(importsIndex: ImportsIndex, source: SourceFile): ImportsIndex = {
    val sourceIndex = importsIndex.sourceIndex
    val updatedSourcesIndex = updateIndexForRemovedSources(sourceIndex, source :: Nil)
    importsIndex.copy(sourceIndex = updatedSourcesIndex)
  }
}

