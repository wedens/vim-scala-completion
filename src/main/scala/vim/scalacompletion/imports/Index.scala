package vim.scalacompletion.imports

import akka.actor.Actor

object Index {
  case class Add(classesInPackages: Map[String, Seq[String]])
  case class RequestPackagesForClass(className: String)
}

class Index extends Actor {
  import Index._
  private var classesInPackages: Map[String, Seq[String]] = Map.empty

  def receive = {
    case Add(portion) => classesInPackages ++= portion
    case RequestPackagesForClass(className) => sender ! classesInPackages.getOrElse(className, Seq.empty)
  }
}

object ClassNameFilter extends (String => Boolean) {
  def apply(fileName: String): Boolean = {
    fileName.endsWith(".class") &&
    // anonymous classes
    !fileName.contains("$$") &&
    // scala 'object'
    !fileName.endsWith("$.class") &&
    // some weird dupolicate
    !fileName.endsWith("$class.class") &&
    // scala package class
    !fileName.endsWith("package.class")
  }
}

object FilePathToFQCNTransformer extends (String => String) {
  def apply(filePath: String): String = {
    val replacedSeparator = filePath.replaceAll("/", "\\.")
    replacedSeparator
      .substring(0, replacedSeparator.lastIndexOf('.'))
      .replaceAll("\\$", "\\.")
  }
}

import java.io.{FileInputStream, File}
import java.util.jar.JarEntry
import java.util.jar.JarInputStream
import scala.util.Try

object JarStream {
  def withJarStream[T](file: File)(f: Stream[JarEntry] => T): T = {
    def loop(jis: JarInputStream): Stream[JarEntry] = {
      val entry = jis.getNextJarEntry
      if (entry == null) {
        Try(jis.close)
        Stream.empty
      } else {
        entry #:: loop(jis)
      }
    }

    val jis = new JarInputStream(new FileInputStream(file))
    try {
      f(loop(jis))
    } finally {
      jis.close
    }
  }
}

import scala.collection.JavaConversions._
import java.nio.file.DirectoryStream
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

object RecursiveDirectoryStream {
  private def recursiveFileListStream(
    dirsToVisit: List[Path],
    currentIterator: Iterator[Path] = Iterator.empty,
    currentStream: Option[DirectoryStream[Path]] = None): Stream[Path] = {

    if (currentIterator.hasNext) {
      val path = currentIterator.next()
      if (Files.isDirectory(path)) {
        recursiveFileListStream(dirsToVisit :+ path, currentIterator)
      } else {
        path #:: recursiveFileListStream(dirsToVisit, currentIterator)
      }
    } else {
      currentStream.foreach(_.close())
      dirsToVisit match {
        case x :: xs =>
          val stream = Files.newDirectoryStream(x)
          recursiveFileListStream(xs, stream.iterator, Some(stream))
        case Nil => Stream.empty
      }
    }
  }

  def apply(path: Path): Stream[Path] =
    recursiveFileListStream(path :: Nil)
}
