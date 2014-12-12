package vim.scalacompletion.filesystem

import java.io.{File => JFile}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

// class ScalaSourcesFinder {

//   private val scalaSourceRegex = """.*\.scala$""".r

//   def findIn(directories: List[JFile]): Seq[JFile] = {
//     findSourcesRecursive(directories)
//   }

//   def isScalaSource(file: java.io.File): Boolean = {
//     file.isFile && scalaSourceRegex.findFirstIn(file.getName).isDefined
//   }

//   @scala.annotation.tailrec
//   private def findSourcesRecursive(directories: List[JFile], sources: Seq[JFile] = Seq.empty): Seq[JFile] = {
//     directories match {
//       case dir :: tail =>
//         val (dirsInDir, filesInDir) = dir.listFiles.partition(_.isDirectory)
//         val sourcesInDir = filesInDir.filter(isScalaSource)
//         findSourcesRecursive(tail ++ dirsInDir, sources ++ sourcesInDir)
//       case Nil => sources
//     }
//   }
// }

trait SourceFinderModule {
  def findSourcesIn(directories: Set[Path]): Set[Path] = {
    directories.flatMap(findSourcesIn)
  }

  def findSourcesIn(directory: Path): Set[Path] = {
    val visitor = new SourceVisitor()
    Files.walkFileTree(directory, visitor)
    visitor.sources
  }

  private val scalaSourceRegex = """.*\.scala$""".r

  def isScalaSource(filePath: Path): Boolean =
    Files.isRegularFile(filePath) &&
      scalaSourceRegex.findFirstIn(filePath.getFileName.toString).isDefined

  private class SourceVisitor extends SimpleFileVisitor[Path] {
    var sources = Set.empty[Path]

    override def visitFile(filePath: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (isScalaSource(filePath)) {
        sources = sources + filePath
      }
      FileVisitResult.CONTINUE
    }
  }
}
