package vim.scalacompletion

import java.io.{File => JFile}
import collection.JavaConversions._

class ScalaSourcesFinder {

  private val scalaSourceRegex = """.*\.scala$""".r

  def findIn(directories: Seq[JFile]): Seq[JFile] = {
    findSourcesRecursive(directories)
  }

  private def findSourcesRecursive(directories: Seq[JFile], sources: Seq[JFile] = Seq.empty): Seq[JFile] = {
    directories match {
      case dir :: tail =>
        val (dirsInDir, filesInDir) = dir.listFiles.partition(_.isDirectory)
        val sourcesInDir = filesInDir.filter(file => scalaSourceRegex.findFirstIn(file.getName).isDefined)
        findSourcesRecursive(tail ++ dirsInDir, sources ++ sourcesInDir)
      case Nil => sources
    }
  }
}
