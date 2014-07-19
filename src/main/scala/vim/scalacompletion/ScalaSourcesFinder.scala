package vim.scalacompletion

import java.io.{File => JFile}
import collection.JavaConversions._

class ScalaSourcesFinder {

  private val scalaSourceRegex = """.*\.scala$""".r

  def findIn(directories: List[JFile]): Seq[JFile] = {
    findSourcesRecursive(directories)
  }

  private def findSourcesRecursive(directories: List[JFile], sources: Seq[JFile] = Seq.empty): Seq[JFile] = {
    directories match {
      case dir :: tail =>
        val (dirsInDir, filesInDir) = dir.listFiles.partition(_.isDirectory)
        val sourcesInDir = filesInDir.filter(file => scalaSourceRegex.findFirstIn(file.getName).isDefined)
        findSourcesRecursive(tail ++ dirsInDir, sources ++ sourcesInDir)
      case Nil => sources
    }
  }
}
