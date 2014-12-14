package vim.scalacompletion.imports

import java.nio.file.{Files, Path}

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

trait ClassFileIndexModule extends FqcnCollector {
  implicit val indexInstance = new Monoid[ClassFileIndex] {
    override def zero: ClassFileIndex = ClassFileIndex()
    override def append(i1: ClassFileIndex, i2: => ClassFileIndex): ClassFileIndex = i1 merge i2
  }

  def buildIndexFromClassFiles(classpath: Set[Path]): Task[ClassFileIndex] = {
    val indexReducer = Reducer.unitReducer[ClassFileIndex, ClassFileIndex](identity)
    Task.reduceUnordered(classpath.toSeq.map { cp =>
      Task.fork(Task {
        val stream = collectFqcnsFrom(cp)
        ClassFileIndex.fromSet(stream.toSet)
      })
    })(indexReducer)
  }
}

object ClassFileIndex {
  def fromSet(fqcns: Set[FQCN]): ClassFileIndex = ClassFileIndex(fqcns.groupBy(_.className).mapValues(_.map(_.scope)))
}

case class ClassFileIndex(private val index: Map[String, Set[String]] = Map.empty) {
  def lookup(className: String): Set[String] =
    index.getOrElse(className, Set.empty)

  def merge(other: ClassFileIndex): ClassFileIndex = copy(index = index |+| other.index)
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
      collectFqcnsFromJar(path)
    }

  lazy val collectFqcnsFromJar: Path => Stream[FQCN] = path =>
    fqcnStreamFromFileNameStream(createFilePathStreamFromJar(path))

  lazy val collectFqcnsFromDir: Path => Stream[FQCN] = path =>
    fqcnStreamFromFileNameStream(createFilePathStreamFromDir(path))
}

