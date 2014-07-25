package vim.scalacompletion.filesystem

import java.io.{File => JFile}

class ScalaSourcesFinder {

  private val scalaSourceRegex = """.*\.scala$""".r

  def findIn(directories: List[JFile]): Seq[JFile] = {
    findSourcesRecursive(directories)
  }

  def isScalaSource(file: java.io.File): Boolean = {
    file.isFile && scalaSourceRegex.findFirstIn(file.getName).isDefined
  }

  @scala.annotation.tailrec
  private def findSourcesRecursive(directories: List[JFile], sources: Seq[JFile] = Seq.empty): Seq[JFile] = {
    directories match {
      case dir :: tail =>
        val (dirsInDir, filesInDir) = dir.listFiles.partition(_.isDirectory)
        val sourcesInDir = filesInDir.filter(isScalaSource)
        findSourcesRecursive(tail ++ dirsInDir, sources ++ sourcesInDir)
      case Nil => sources
    }
  }
}
