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

class Projects extends Actor with All {
  import Projects._

  private var projectsStates = List.empty[ProjectState]

  private def findProjectStateFor(path: Path): Option[ProjectState] =
    projectsStates.find(ps => path.startsWith(ps.projectPath))

  def receive = {
    case InitProject(path) =>
      projectManagement.init(path) match {
        case -\/(ex) => sender ! ex
        case \/-(projectState) =>
          projectsStates = projectState :: projectsStates
          sender ! projectState
      }

    case CalculatePackageForFile(path) =>
      findProjectStateFor(path).map { projectState =>
        sender ! projectApi.calculatePackageForFile(path).run(projectState)
      } getOrElse {
        sender ! new RuntimeException("Project not found")
      }

    case FindDeclaration(position) =>
      findProjectStateFor(position.sourcePath).map { projectState =>
        sender ! projectApi.findDeclaration(position).run(projectState)
      } getOrElse {
        sender ! new RuntimeException("Project not found")
      }

    case Complete(position, prefix) =>
      //TODO: some VirtualFile concept?
      findProjectStateFor(Paths.get(position.sourceName)).map { projectState =>
        sender ! projectApi.completeAt(position, prefix).run(projectState)
      } getOrElse {
        sender ! new RuntimeException("Project not found")
      }

    case SuggestImportsForClass(path, className) =>
      findProjectStateFor(path).map { projectState =>
        sender ! projectApi.suggestImportsForClass(className).run(projectState)
      } getOrElse {
        sender ! new RuntimeException("Project not found")
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
        lazy val compiler = createCompiler(config.classpath)
        val (_, importsIndex) = (for {
          sources <- sourcesManagement.loadAllSourcesFrom(config.sourcesDirs)
          importsIndex <- createImportsIndex(config.classpath + javaLib, sources)
        } yield (sources, importsIndex)).run(compiler)
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

trait ProjectApiModule { self: DeclarationFinderModule with PackageCalculationModule with CompletionModule =>
  lazy val projectApi = new ProjectApi
  class ProjectApi {
    def suggestImportsForClass(className: String): Reader[ProjectState, Set[String]] =
      Reader((projectState: ProjectState) => projectState.importsIndex.lookupForClass(className))

    def calculatePackageForFile(path: Path): Reader[ProjectState, Option[String]] =
      Reader(ps => packageCalculator.calculatePackageFor(ps.config.sourcesDirs, path))

    def findDeclaration(position: PositionInSource): Reader[ProjectState, PositionInSource] =
      declarationFinder.findSymbolDeclaration(position) <=< Reader(_.compiler)

    def completeAt(position: PositionInSource, prefix: Option[String]): Reader[ProjectState, Seq[MemberInfo]] =
      completion.complete(position, prefix) <=< Reader(_.compiler)
  }
}

trait SourcesManagementModule { self: SourcesFinderModule with CompilerModule =>
  lazy val sourcesManagement = new SourcesManagement

  class SourcesManagement {
    def loadAllSourcesFrom(directoriesWithSources: Set[Path]): Reader[Compiler, Seq[SourceFile]] =
      Reader { compiler =>
        val sourcesPaths = sourcesFinder.findSourcesIn(directoriesWithSources).toList
        val sources = sourcesPaths.map { source =>
          //TODO: dup
          new BatchSourceFile(AbstractFile.getFile(source.toFile))
        }

        compiler.reloadSources(sources)
        sources
      }

    def loadSource(sourceName: String, sourcePath: Path): Reader[Compiler, SourceFile] =
      Reader { compiler =>
        //TODO: move
        val source = new BatchSourceFile(sourceName, AbstractFile.getFile(sourcePath.toFile).toCharArray)
        compiler.reloadSources(source :: Nil)
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
    def findSymbolDeclaration(position: PositionInSource): Reader[Compiler, PositionInSource] =
      Reader { compiler =>
        val source = new BatchSourceFile(AbstractFile.getFile(position.sourcePath.toFile)) // TODO: dup
        // TODO: dup
        val lineOffset = source.lineToOffset(position.lineIdx)
        val positionToFind = source.position(lineOffset + position.columnIdx)
        //TODO: handle if not in source
        val foundPosition = compiler.findDeclarationOfSymbolAt(positionToFind)
        PositionInSource(
          Paths.get(foundPosition.source.path),
          foundPosition.line,
          foundPosition.column
        )
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
