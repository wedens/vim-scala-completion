package vim.scalacompletion.imports

import scala.collection.JavaConversions._
import java.nio.file.DirectoryStream
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try

// TODO: stream left open when exception occured
// TODO: unified error handling during stream processing
object RecursiveDirectoryStream {
  def apply(path: Path): Stream[Path] =
    recursiveFileListStream(path :: Nil)

  private def recursiveFileListStream(
    dirsToVisit: List[Path],
    currentIterator: Iterator[Path] = Iterator.empty,
    currentStream: Option[DirectoryStream[Path]] = None): Stream[Path] = {

    if (currentIterator.hasNext) {
      val path = currentIterator.next()
      if (Files.isDirectory(path)) {
        path #:: recursiveFileListStream(dirsToVisit :+ path, currentIterator)
      } else {
        path #:: recursiveFileListStream(dirsToVisit, currentIterator)
      }
    } else {
      currentStream.foreach(s => Try(s.close()))
      dirsToVisit match {
        case x :: xs =>
          val stream = Files.newDirectoryStream(x)
          recursiveFileListStream(xs, stream.iterator, Some(stream))
        case Nil => Stream.empty
      }
    }
  }
}
