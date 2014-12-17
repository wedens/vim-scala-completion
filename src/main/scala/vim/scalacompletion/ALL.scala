package vim.scalacompletion

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._
import vim.scalacompletion.compiler.{CompilerModule, Compiler}
import vim.scalacompletion.completion.{MemberInfo, MemberInfoModule, CompletionModule, CompletionTypeDetectionModule}
import vim.scalacompletion.config.{ProjectConfig, ConfigurationModule}
import vim.scalacompletion.imports._

import collection.JavaConversions._

import com.typesafe.config.{ConfigFactory, Config}

import scala.math
import scala.reflect.internal.util.{BatchSourceFile, SourceFile, Position}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.interactive.Global
import akka.actor.Actor
import akka.actor.Status.Failure
import scalaz._
import Scalaz._

case class ProjectState(projectPath: Path, compiler: Compiler, importsIndex: ImportsIndex, config: ProjectConfig)

trait All
  extends ProjectManagement
  with ProjectApiModule
  with ConfigurationModule
  with CompilerModule
  with SourcesManagementModule
  with ImportsIndexModule
  with ClassFileIndexModule
  with SourceIndexModule
  with SourcesFinderModule
  with DeclarationFinderModule
  with PackageCalculationModule
  with CompletionModule
  with CompletionTypeDetectionModule
  with MemberInfoModule

object PositionInSource {
  def apply(sourcePath: Path, lineIdx: Int, columnIdx: Int) =
    new PositionInSource(sourcePath.toString, sourcePath, lineIdx, columnIdx)
}
case class PositionInSource(sourceName: String, sourcePath: Path, lineIdx: Int, columnIdx: Int)

object Projects {
  case class InitProject(path: Path)
  case class CalculatePackageForFile(path: Path)
  case class FindDeclaration(position: PositionInSource)
  case class Complete(position: PositionInSource, prefix: Option[String])
  case class SuggestImportsForClass(path: Path, className: String)
}

case object ProjectNotFound extends Exception("Project not found")

class Projects extends Actor with All {
  import Projects._

  private var projectsStates = List.empty[ProjectState]

  private def withProjectState[T](path: Path)(f: ProjectState => T) =
    projectsStates
      .find(ps => path.startsWith(ps.projectPath))
      .map(f)
      .getOrElse {
        sender ! Failure(ProjectNotFound)
      }

  def receive = {
    case InitProject(path) =>
      projectManagement.init(path) match {
        case -\/(ex) => sender ! Failure(ex)
        case \/-(projectState) =>
          projectsStates = projectState :: projectsStates
          sender ! projectState
      }

    case CalculatePackageForFile(path) =>
      withProjectState(path) { projectState =>
        sender ! projectApi.calculatePackageForFile(projectState, path)
      }

    case FindDeclaration(position) =>
      withProjectState(position.sourcePath) { projectState =>
        sender ! projectApi.findDeclaration(projectState, position)
      }

    case Complete(position, prefix) =>
      //TODO: some VirtualFile concept?
      withProjectState(Paths.get(position.sourceName)) { projectState =>
        sender ! projectApi.completeAt(projectState, position, prefix)
      }

    case SuggestImportsForClass(path, className) =>
      withProjectState(path) { projectState =>
        sender ! projectApi.suggestImportsForClass(projectState, className)
      }
  }
}

trait ProjectManagement { self: ConfigurationModule with CompilerModule with SourcesManagementModule with ImportsIndexModule  =>

  lazy val projectManagement = new ProjectManagement
  class ProjectManagement {
    def init(filePath: Path): Throwable \/ ProjectState =
      for {
        projectRoot <- findProjectRoot(filePath) >>= (_.toRightDisjunction(new RuntimeException("Project not found")))
        config <- configReader.parseConfig(projectRoot.resolve(configFileName))
        javaLib <- javaLib
      } yield {
        val compiler = createCompiler(config.classpath)
        val sources = sourcesManagement.loadAllSourcesFrom(compiler, config.sourcesDirs)
        val importsIndex = createImportsIndex(compiler, config.classpath + javaLib, sources)
        ProjectState(projectRoot, compiler, importsIndex, config)
      }

    private lazy val configFileName = Paths.get("vim_scala_completion.conf")

    private lazy val javaLib: Throwable \/ Path =
      System.getProperty("sun.boot.class.path")
        .split(":")
        .find(_.endsWith("rt.jar"))
        .map(Paths.get(_))
        .toRightDisjunction(new RuntimeException("rt.jar not found"))

    private def findProjectRoot(fromPath: Path): Throwable \/ Option[Path] = {
      lazy val fromDir = if (Files.isDirectory(fromPath)) fromPath else fromPath.getParent
      lazy val containsConfig = \/.fromTryCatchNonFatal(Loan.loan(Files.newDirectoryStream(fromDir)).to { dirStream =>
        dirStream.iterator().exists(_.getFileName.startsWith(configFileName))
      })

      if (fromPath == null) {
        none.right
      } else {
        containsConfig match {
          case -\/(ex) => ex.left
          case \/-(b) if b => fromPath.some.right
          case \/-(b) => findProjectRoot(fromPath.getParent)
        }
      }
    }
  }
}

trait ProjectApiModule {
  self: DeclarationFinderModule
    with PackageCalculationModule
    with CompletionModule
    with SourcesManagementModule
    with ImportsIndexModule =>

  type ProjectRequest[A] = Reader[ProjectState, A]
  lazy val projectApi = new ProjectApi
  class ProjectApi {
    def suggestImportsForClass(projectState: ProjectState, className: String): Set[String] =
      projectState.importsIndex.lookupForClass(className)

    def calculatePackageForFile(projectState: ProjectState, path: Path): Option[String] =
      packageCalculator.calculatePackageFor(projectState.config.sourcesDirs, path)

    def findDeclaration(projectState: ProjectState, position: PositionInSource): Option[PositionInSource] =
      declarationFinder.findSymbolDeclaration(projectState.compiler, position)

    def completeAt(projectState: ProjectState, position: PositionInSource, prefix: Option[String]): Seq[MemberInfo] =
      completion.complete(projectState.compiler, position, prefix)

    def sourceCreated(projectState: ProjectState, path: Path): ProjectState =
      sourceModified(projectState, path)

    def sourceRemoved(projectState: ProjectState, path: Path): ProjectState = {
      val compiler = projectState.compiler
      val source = sourcesManagement.removeSource(compiler, path)
      val index = projectState.importsIndex
      val updatedIndex = removeSourceFromIndex(index, source)
      projectState.copy(importsIndex = index)
    }

    def sourceModified(projectState: ProjectState, path: Path): ProjectState = {
      val compiler = projectState.compiler
      val source = sourcesManagement.loadSource(compiler, path)
      val index = projectState.importsIndex
      val updatedIndex = updateSourceInIndex(index, source)
      projectState.copy(importsIndex = index)
    }
  }
}

trait SourcesManagementModule { self: SourcesFinderModule with CompilerModule =>
  lazy val sourcesManagement = new SourcesManagement

  class SourcesManagement {
    def loadAllSourcesFrom(compiler: Compiler, directoriesWithSources: Set[Path]): Seq[SourceFile] = {
      val sourcesPaths = sourcesFinder.findSourcesIn(directoriesWithSources).toList
      val sources = sourcesPaths.map { source =>
        //TODO: dup
        new BatchSourceFile(AbstractFile.getFile(source.toFile))
      }

      compiler.reloadSources(sources)
      sources
    }

    def loadSource(compiler: Compiler, sourcePath: Path): SourceFile = {
      //TODO: move
      val source = new BatchSourceFile(AbstractFile.getFile(sourcePath.toFile))
      compiler.reloadSources(source :: Nil)
      source
    }

    def loadSource(compiler: Compiler, sourceName: String, sourcePath: Path): SourceFile = {
      //TODO: move
      val source = new BatchSourceFile(sourceName, AbstractFile.getFile(sourcePath.toFile).toCharArray)
      compiler.reloadSources(source :: Nil)
      source
    }

    def removeSource(compiler: Compiler, sourcePath: Path): SourceFile = {
      //TODO: dup
      val source = new BatchSourceFile(AbstractFile.getFile(sourcePath.toFile))
      compiler.removeSources(source :: Nil)
      source
    }
  }
}

trait SourcesFinderModule {
  lazy val sourcesFinder = new SourcesFinder
  class SourcesFinder {
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
}

trait DeclarationFinderModule { self: CompilerModule with CompilerModule =>
  lazy val declarationFinder = new DeclarationFinder

  class DeclarationFinder {
    def findSymbolDeclaration(compiler: Compiler, position: PositionInSource): Option[PositionInSource] = {
      val source = new BatchSourceFile(AbstractFile.getFile(position.sourcePath.toFile)) // TODO: dup
      // TODO: dup
      val lineOffset = source.lineToOffset(position.lineIdx)
      val positionToFind = source.position(lineOffset + position.columnIdx)
      //TODO: handle if not in source
      compiler.findDeclarationOfSymbolAt(positionToFind).map { foundPosition =>
        PositionInSource(
          Paths.get(foundPosition.source.path),
          foundPosition.line,
          foundPosition.column
        )
      }
    }
  }
}

trait PackageCalculationModule {
  lazy val packageCalculator = new PackageCalculator
  class PackageCalculator {
    def calculatePackageFor(sourcesDirs: Set[Path], file: Path): Option[String] =
      sourcesDirs.find(file.startsWith).map { sourceDirForFile =>
        val sourcePathRelative = sourceDirForFile.relativize(file)
        val withoutFileName = sourcePathRelative.getParent
        val pkgName = withoutFileName.toString.replaceAll("/", ".")
        pkgName
      }
  }
}

import java.io._

class Loan[A <: AutoCloseable](resource: => A) {
  def to[B](block: A => B) = {
    lazy val r = resource
    var t: Throwable = null
    try {
      block(r)
    } catch {
      case x => t = x; throw x
    } finally {
      if (r != null) {
        if (t != null) {
          try {
            r.close()
          } catch {
            case y => t.addSuppressed(y)
          }
        } else {
          r.close()
        }
      }
    }
  }
}

object Loan {
  def loan[A <: AutoCloseable](resource: => A) = new Loan(resource)
}
