import java.io.FileInputStream
import java.util.jar.JarEntry
import java.util.jar.JarInputStream
import scala.annotation.tailrec

object JarTest extends App {

  def fileNameFilter(fileName: String): Boolean = {
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

  def fileNameToFqcn(fileName: String): String = {
    val replacedPath = fileName.replaceAll("/", "\\.")
    replacedPath.substring(0, replacedPath.lastIndexOf('.')).replaceAll("\\$", "\\.")
  }

  def jarStream(stream: JarInputStream): Stream[JarEntry] = {
    val entry = stream.getNextJarEntry
    if (entry == null) {
      Stream.empty
    } else {
      entry #:: jarStream(stream)
    }
  }

  def classFiles(stream: JarInputStream) =
    jarStream(stream)
      .filter(f => fileNameFilter(f.getName))
      .map(f => fileNameToFqcn(f.getName))

  (
    "/home/wedens/.ivy2/cache/org.scalaz/scalaz-core_2.11/jars/scalaz-core_2.11-7.1.0.jar" ::
    "/home/wedens/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.4.jar" ::
    "/home/wedens/.ivy2/cache/org.springframework/spring-core/jars/spring-core-3.2.12.RELEASE.jar" ::
    "/home/wedens/.ivy2/cache/com.h2database/h2/jars/h2-1.3.173.jar" ::
    "/home/wedens/.ivy2/cache/org.eclipse.persistence/eclipselink/jars/eclipselink-2.5.2.jar" ::
    Nil
  ).par.foreach { j =>
    val jarIs = new JarInputStream(new FileInputStream(j))
    classFiles(jarIs).foreach(_ => {})
    jarIs.close()
  }
  // val jarIs = new JarInputStream(new FileInputStream("/home/wedens/.ivy2/cache/org.specs2/specs2-core_2.11/jars/specs2-core_2.11-2.4.11.jar"))
  // classFiles(jarIs).foreach(println)
  // jarIs.close()

  import scala.collection.JavaConversions._
  import java.nio.file.DirectoryStream
  import java.nio.file.Files
  import java.nio.file.Path
  import java.nio.file.Paths

  def recursiveFileListStream(
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

  def recursiveFileListStream(dir: String): Stream[Path] =
    recursiveFileListStream(Paths.get(dir) :: Nil, Iterator.empty, None)

  val dir = "/home/wedens/Projects/vim-scala-completion/target/scala-2.11/classes"
  // recursiveFileListStream(dir)
  //   .map(_.toAbsolutePath.toString.drop(dir.length + 1))
  //   .filter(fileNameFilter)
  //   .map(fileNameToFqcn)
  //   .foreach(println)

  // (dir ::
  // "/home/wedens/Projects/ui-gen/target/scala-2.11/classes" ::
  // "/home/wedens/Projects/zenith-portal/regserver/target/classes" ::
  // "/home/wedens/Projects/zenith-portal/client/target/classes" ::
  // "/home/wedens/Projects/zenith-portal/admin_client/target/classes" ::
  // "/home/wedens/Projects/zenith-portal/model/target/classes" ::
  // "/home/wedens/Projects/scala/build/quick/classes" ::
  // Nil).foreach(recursiveFileListStream(_).foreach(x => {}))
}
