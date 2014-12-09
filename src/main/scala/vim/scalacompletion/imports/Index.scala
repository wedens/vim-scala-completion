package vim.scalacompletion.imports

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorRefFactory
import akka.actor.Props
import scala.collection.JavaConversions._
import java.nio.file.DirectoryStream
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global

trait IndexBuilder extends FqcnCollector {
  lazy val buildIndex: ActorRefFactory => Set[Path] => Future[Index] =
    actorFactory => paths => {
      val indexPromise = Promise[Index]
      //TODO: handle timeout
      val collectionProcessManager = actorFactory.actorOf(Props(new Actor {
        private var index = Index()
        private var left = paths

        def receive = {
          case (fqcns: List[FQCN], path: Path) =>
            index = index merge fqcns
            left = left - path
            if (left.isEmpty) {
              indexPromise success index
              context.stop(self)
            }
        }
      }))
      paths.foreach { path =>
        Future {
          try {
            val stream = collectFqcnsFrom(path)
            collectionProcessManager ! (stream.toList, path)
          } catch {
            case ex: Exception => indexPromise failure ex
          }
        }
      }
      indexPromise.future
    }
}

case class Index(private val index: Map[String, Set[String]] = Map.empty) {
  def lookup(className: String): Set[String] =
    index.getOrElse(className, Set.empty)

  def merge(fqcns: Seq[FQCN]): Index = {
    Index(fqcns.foldLeft(index) {
      case (result, FQCN(scope, className)) =>
        result + (className -> (result.getOrElse(className, Set.empty) + scope))
    })
  }
}

case class FQCN(scope: String, className: String)

trait FqcnCollection {
  protected lazy val createFilePathStreamFromJar: Path => Stream[String] =
    JarStream(_).filter(!_.isDirectory).map(_.getName)

  protected lazy val createFilePathStreamFromDir: Path => Stream[String] = path => {
    lazy val basePathAbsolute = path.toAbsolutePath.toString
    RecursiveDirectoryStream(path)
      .filterNot(p => Files.isDirectory(p))
      // remove base directory and separator
      .map(_.toAbsolutePath.toString.drop(basePathAbsolute.length + 1))
  }

  protected lazy val fqcnStreamFromFileNameStream: Stream[String] => Stream[FQCN] =
   _.filter(filterClassFiles)
    .map(filePathToFqcn)
    .map(fqcnStringToObject)

  protected lazy val filterClassFiles: String => Boolean = fileName => {
    fileName.endsWith(".class") &&
    // anonymous classes
    !fileName.contains("$$") &&
    // scala 'object'
    !fileName.endsWith("$.class") &&
    // some weird duplicate
    !fileName.endsWith("$class.class") &&
    // scala package class
    !fileName.endsWith("package.class")
  }

  protected lazy val filePathToFqcn: String => String = path => {
    // replace path separator with dot
    val replacedSeparator = path.replaceAll("/", "\\.")
    replacedSeparator
      // trim file extension
      .substring(0, replacedSeparator.lastIndexOf('.'))
      // replace nested classes separator with dot
      .replaceAll("\\$", "\\.")
  }

  protected lazy val fqcnStringToObject: String => FQCN = fqcn => {
    fqcn.splitAt(fqcn.lastIndexOf('.')) match {
      case (scope, className) =>
        // remove leading dot from class name if class is in package
        FQCN(scope, className.dropWhile(_ == '.'))
    }
  }
}

trait FqcnCollector extends FqcnCollection {
  lazy val collectFqcnsFrom: Path => Stream[FQCN] = path =>
    if (Files.isDirectory(path)) {
      collectFqcnsFromDir(path)
    } else {
      collectFqcnsFromDir(path)
    }

  lazy val collectFqcnsFromJar: Path => Stream[FQCN] = path =>
    fqcnStreamFromFileNameStream(createFilePathStreamFromJar(path))

  lazy val collectFqcnsFromDir: Path => Stream[FQCN] = path =>
    fqcnStreamFromFileNameStream(createFilePathStreamFromDir(path))
}

import java.io.{FileInputStream, File}
import java.util.jar.JarEntry
import java.util.jar.JarInputStream
import scala.util.Try

// TODO: unified error handling during stream processing
object JarStream {
  def apply(path: Path): Stream[JarEntry] = {
    def loop(jis: JarInputStream): Stream[JarEntry] = {
      val entry = jis.getNextJarEntry
      if (entry == null) {
        Try(jis.close)
        Stream.empty
      } else {
        entry #:: loop(jis)
      }
    }
    val jis = new JarInputStream(new FileInputStream(path.toFile))
    loop(jis)
  }
}


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
