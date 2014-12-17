package vim.scalacompletion.imports

import java.nio.file.Path

import scala.tools.nsc.interactive.Global
import scalaz._
import scalaz.std.set._
import scalaz.syntax.semigroup._
import vim.scalacompletion.compiler.{FqcnsCollectorFromTree, Compiler}
import scala.reflect.internal.util.SourceFile
import akka.actor.Actor

case class SourceFQCN(scope: String, className: String, file: String)

object SourceIndex {
  def fromSet(fqcns: Set[SourceFQCN]): SourceIndex = SourceIndex(fqcns)
}

case class SourceIndex(private val index: Set[SourceFQCN] = Set.empty) {
  def lookup(className: String): Set[String] =
    index.filter(_.className == className).map(_.scope)

  def removeSource(fileName: String): SourceIndex =
    copy(index = index.filterNot(_.file == fileName))

  def merge(other: SourceIndex): SourceIndex = copy(index = index |+| other.index)
}

trait SourceIndexModule extends FqcnsCollectorFromTree {
  def createIndexForSources(compiler: Compiler, sources: Seq[SourceFile]): SourceIndex =
    sources.foldLeft(SourceIndex()) {
      case (idx, source) =>
        val tree = compiler.typedTreeForSource(source)
        val fqcns = createFqcnsFromTreeCollector(compiler)(tree)
        val sourceIndex = SourceIndex.fromSet(fqcns)
        idx.merge(sourceIndex)
    }

  def updateIndexForUpdatedSources(compiler: Compiler, index: SourceIndex, sources: Seq[SourceFile]): SourceIndex =
    sources.foldLeft(index) {
      case (idx, source) =>
        val removedSource = index.removeSource(source.path)
        val tree = compiler.typedTreeForSource(source)
        val fqcns = createFqcnsFromTreeCollector(compiler)(tree)
        val sourceIndex = SourceIndex.fromSet(fqcns)
        removedSource.merge(sourceIndex)
    }

  def updateIndexForRemovedSources(index: SourceIndex, sources: Seq[SourceFile]): SourceIndex =
    sources.foldLeft(index) {
      case (idx, source) => index.removeSource(source.path)
    }
}

//import vim.scalacompletion.filesystem.FileSystemEvents._
//import vim.scalacompletion.compiler.SourceManagement
//import java.nio.file.Path
//
//class SourceFilesMonitor(watchService: WatchService, sourcesDirs: Set[Path],
//  compiler: SourceManagement) extends Actor { _: SourceFinderModule =>
//
//  watchService.addObserver(self)
//  sourcesDirs.foreach(watchService.watchRecursively)
//  findSourcesIn(sourcesDirs)
//
//  def receive = {
//    case Created(file) if isScalaSource(file.toPath) =>
//    case Modified(file) if isScalaSource(file.toPath) =>
//    case Deleted(file) if isScalaSource(file.toPath) =>
//  }
//}
//
//
//import scala.reflect.internal.util.SourceFile
//import scala.reflect.internal.util.BatchSourceFile
//import scala.reflect.io.AbstractFile
//
//trait SourceFileCreation {
//  def toSourceFile(filePath: Path): SourceFile =
//    new BatchSourceFile(AbstractFile.getFile(filePath.toFile))
//}
