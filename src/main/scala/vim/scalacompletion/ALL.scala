package vim.scalacompletion

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._
import vim.scalacompletion.compiler.Compiler
import vim.scalacompletion.imports.{SourceIndexModule, ClassFileIndexModule, ClassFileIndex, SourceIndex}

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

case class ProjectConfig(classpath: Set[Path], sourcesDirs: Set[Path])

case class ImportsIndex(sourceIndex: SourceIndex, classFileIndex: ClassFileIndex) {
  def lookupForClass(className: String): Set[String] =
    sourceIndex.lookup(className) ++ classFileIndex.lookup(className)
}

case class MemberInfo(name: String, fullSignature: String,
  isConstructor: Boolean = false, isLocal: Boolean = false,
  isPublic: Boolean = false, isFromRootObjects: Boolean = true,
  isInherited: Boolean = false, isAccessible: Boolean = true)

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

trait CompletionTypeDetectionModule {
  sealed trait CompletionType
  case object Scope extends CompletionType
  case object Type extends CompletionType
  case object NoCompletion extends CompletionType

  lazy val completionTypeDetector = new CompletionTypeDetector
  class CompletionTypeDetector {
    private val scopeKeywords = Seq("if", "else", "case", "new", "yield", "extends",
      "with", "class", "trait", "val", "var", "def").map(_.reverse)

    def detectAt(line: String, pos: Int): CompletionType = {
      val (beforePosAndPos, afterPos) = line.splitAt(pos + 1)
      val atPos = if (beforePosAndPos.nonEmpty) beforePosAndPos.last else ""
      if (pos >= line.length && atPos == '.') { // TODO: this is a dirty hack
        Type
      } else {
        val beforePos = if (beforePosAndPos.nonEmpty) beforePosAndPos.init else ""
        val lineBeforePosReversed = beforePos.reverse

        val notEscapedQuoteRegex = "\"(\\\\\"|[^\"])*".r
        val matches = notEscapedQuoteRegex.findAllMatchIn(beforePos).toSeq
        val balanced = matches.length % 2 == 0
        val insideOfString = !balanced
        if (insideOfString) {
          if (lineBeforePosReversed.startsWith("$")) {
            Scope
          } else {
            val openingQuotePosition = matches.last.start
            val interpolatedExprIdx = beforePos.indexOfSlice("${", openingQuotePosition)
            if (interpolatedExprIdx == -1) {
              NoCompletion
            } else {
              val exprStart = interpolatedExprIdx + 2
              val expr = beforePos.drop(exprStart)
              val posInExpr = pos - exprStart
              detectAt(expr, posInExpr)
            }
          }
        } else {
          detectInExpr(lineBeforePosReversed)
        }
      }
    }

    private def detectInExpr(line: String) = {
      val trimmed = line.trim
      trimmed.headOption match {
        // type completion after identifier with following dot
        case Some('.') => Type
        // empty line before completion position
        case None => Scope
        // scope completion after ';' separator
        case Some(';') => Scope
        // after: 'if', 'with' etc
        case Some(_) if precedingKeyword(trimmed) => Scope
        // import without any selector: import
        case Some(_) if emptyImport(line) => Scope
        // inside '{}' in import: import pkg.nest.{}
        case Some(_) if importSelectors(trimmed) => Type
        // complete infix method parameter
        case Some(_) if infixParameter(trimmed) => Scope
        // complete infix members
        case Some(ch) if ch.isLetterOrDigit => Type
        case _ => Scope
      }
    }

    private def isIdentifierChar(ch: String) = {
      val positive = "[\\p{L}0-9\\p{Punct}\\p{Sm}]".r
      val exclude = "[^()\\[\\];.,{}\"'$]".r

      exclude.findFirstIn(ch).isDefined && positive.findFirstIn(ch).isDefined
    }

    private def infixParameter(str: String) = {
      val wordRemoved = str.dropWhile(!_.isSpaceChar)
      val somethingBeforeSpace = wordRemoved.length - 1 > 0
      if (somethingBeforeSpace) {
        val withoutSpace = wordRemoved.tail
        val looksLikeIdentifierBeforeSpace = withoutSpace.head.isLetterOrDigit
        looksLikeIdentifierBeforeSpace
      } else false
    }

    private def precedingKeyword(str: String) = scopeKeywords.exists(str.startsWith)
    private def importSelectors(str: String) = str.matches(".*\\{.* tropmi[\\s;]*")
    private def emptyImport(str: String) = str.matches("\\s+tropmi[\\s;]*")
  }
}

trait MemberInfoModule {
  def memberInfoFrom(global: Global)(member: global.Member): MemberInfo = {
    import global.definitions

    val sym           = member.sym
    val name          = sym.nameString
    val fullSignature = member.forceInfoString
    val isConstructor = sym.isConstructor
    val isLocal       = sym.isLocalToBlock
    val isPublic      = sym.isPublic
    val isInherited   = member match {
      case tm: global.TypeMember => tm.inherited
      case _ => false
    }
    val isFromRootObjects = sym.owner == definitions.AnyClass ||
                            sym.owner == definitions.AnyRefClass ||
                            sym.owner == definitions.ObjectClass

    val isAccessible = member.accessible

    MemberInfo(name, fullSignature,
      isConstructor, isLocal,
      isPublic, isFromRootObjects,
      isInherited, isAccessible)
  }

  def filterMember(prefix: Option[String])(member: MemberInfo): Boolean = {
    lazy val startsWithPrefix = prefix.map(member.name.startsWith)
    !member.isConstructor && member.isAccessible && startsWithPrefix.getOrElse(true)
  }

  def calculateMemberRank(prefix: Option[String])(member: MemberInfo): Int = {
    ((prefix.isDefined, prefix.map(member.name.length - _.length) | 0) ::
    (!member.isInherited, 10) ::
    (member.isLocal, 20) ::
    (member.isPublic, 10) ::
    (!member.isFromRootObjects, 30) :: Nil)
      .foldLeft(0) {
        case (a, (b, r)) if b => a + r
        case (a, _) => a
      }
  }

  def orderByRankDesc(prefix: Option[String]): scala.math.Ordering[MemberInfo]  =
    scala.math.Ordering.by(calculateMemberRank(prefix)(_))
}

trait CompletionModule { self: SourcesManagementModule with CompilerModule with CompletionTypeDetectionModule with MemberInfoModule =>
  lazy val completion = new Completion

  class Completion {
    private def positionInSource(source: SourceFile, lineIdx: Int, columnIdx: Int): Position = {
      val lineOffset = source.lineToOffset(lineIdx)
      source.position(lineOffset + columnIdx)
    }

    private def detectCompletionType(position: Position): CompletionType = {
      val lineContent = position.source.lineToString(position.line)
      completionTypeDetector.detectAt(lineContent, position.column - 1) // TODO: why -1
    }

    def members(completionType: CompletionType, position: Position): Reader[Compiler, Seq[MemberInfo]] =
      Reader { compiler =>
        val memberInfoExtractor = memberInfoFrom(compiler) _
        completionType match {
          case Scope => compiler.typeCompletion(position, memberInfoExtractor)
          case Type =>
            val lastCharPosition = position.point - 1
            compiler.scopeCompletion(position.withPoint(lastCharPosition), memberInfoExtractor)
          case NoCompletion => Seq.empty
        }
      }

  def complete(position: PositionInSource, prefix: Option[String]): Reader[Compiler, Seq[MemberInfo]] =
    for {
      source <- sourcesManagement.loadSource(position.sourceName, position.sourcePath)
      completionPosition = positionInSource(source, position.lineIdx, position.columnIdx)
      completionType = detectCompletionType(completionPosition)
      members <- members(completionType, completionPosition)
    } yield {
      members filter filterMember(prefix) sorted orderByRankDesc(prefix)
    }
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

trait CompilerModule {
  def createCompiler(classpath: Set[Path]): Compiler = Compiler(classpath)
}

trait ConfigurationModule {
  lazy val configReader = new ConfigurationReader
  class ConfigurationReader {
    private def loadConfig(configPath: Path): Throwable \/ Config =
      \/.fromTryCatchNonFatal {
        ConfigFactory.parseFile(configPath.toFile)
      }

    private def readConfig(config: Config): Throwable \/ ProjectConfig =
      for {
        classpath <- \/.fromTryCatchNonFatal(
          config.getStringList("vim.scala-completion.classpath"))
        sourcesDirs <- \/.fromTryCatchNonFatal(
          config.getStringList("vim.scala-completion.src-directories"))
      } yield {
        ProjectConfig(
          classpath.map(Paths.get(_)).toSet,
          sourcesDirs.map(Paths.get(_)).toSet
        )
      }

    def parseConfig(configPath: Path): Throwable \/ ProjectConfig =
      loadConfig(configPath) >>= readConfig
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

trait ImportsIndexModule { self: ClassFileIndexModule with SourceIndexModule with CompilerModule =>
  def createImportsIndex(classpath: Set[Path], sources: Seq[SourceFile]): Reader[Compiler, ImportsIndex] =
    Reader { compiler =>
      val classFileIndex = buildIndexFromClassFiles(classpath).run
      val sourcesIndex = createIndexForSources(sources).run(compiler)
      ImportsIndex(sourcesIndex, classFileIndex)
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
