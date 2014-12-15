package vim.scalacompletion

import java.nio.file.{Files, Paths, Path}
import vim.scalacompletion.compiler.Compiler
import vim.scalacompletion.filesystem.SourceFinderModule
import vim.scalacompletion.imports.{SourceIndexModule, ClassFileIndexModule, ClassFileIndex, SourceIndex}

import collection.JavaConversions._

import com.typesafe.config.{ConfigFactory, Config}

import scala.math
import scala.reflect.internal.util.{BatchSourceFile, SourceFile, Position}
import scala.reflect.io.AbstractFile
import scala.tools.nsc.interactive.Global
import scalaz._
import Scalaz._

trait Project {
  self: Configuraion
    with Comp
    with SourcesLoading
    with ImportsIndexModule
    with CompletionModule
    with DeclarationFinder
    with PackageNameCalculation =>

  case class ProjectState(compiler: Compiler, importsIndex: ImportsIndex, config: ProjectConfig)

  lazy val configFileName = Paths.get("vim_scala_completion.conf")

  def findProjectRoot(fromPath: Path): Throwable \/ Option[Path] = {
    def containsConfig = \/.fromTryCatchNonFatal(Loan.loan(Files.newDirectoryStream(fromPath)).to { dirStream =>
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

  def initializeProjectAt(path: Path): Throwable \/ ProjectState = {
    for {
      projectRoot <- findProjectRoot(path)
        .flatMap(_.toRightDisjunction(new RuntimeException("Project not found")))
      //TODO: rt.jar
      config <- parseConfig(projectRoot.resolve(configFileName))
    } yield {
      val compiler = createCompiler(config.classpath)
      val importsIndex = (for {
        sources <- loadSourcesFrom(config.sourcesDirs)
        importsIndex <- createImportsIndex(config.classpath, sources)
      } yield importsIndex).run(compiler)

      ProjectState(compiler, importsIndex, config)
    }
  }

  def suggestImportsForClass(className: String): Reader[ProjectState, Set[String]] =
    Reader(_.importsIndex.lookupForClass(className))

  def calculatePackageForFile(path: Path): Reader[ProjectState, Option[String]] =
    Reader(ps => calculatePackage(ps.config.sourcesDirs)(path))

  def findDeclaration(sourcePath: Path, lineIdx: Int, columnIdx: Int): Reader[ProjectState, SymbolPosition] =
    Reader((ps: ProjectState) => ps.compiler) >=> findSymbolDeclaration(sourcePath, lineIdx, columnIdx)

  def completeAt(sourceName: String, sourcePath: Path, lineIdx: Int, columnIdx: Int): Reader[ProjectState, Seq[MemberInfo]]
}

case class MemberInfo(name: String, fullSignature: String,
  isConstructor: Boolean = false, isLocal: Boolean = false,
  isPublic: Boolean = false, isFromRootObjects: Boolean = true,
  isInherited: Option[Boolean] = None, isAccessible: Boolean = true)

trait CompletionModule { self: SourcesLoading with ComplectionTypeDetection with MemberInfoExtraction =>
  private def positionInSource(source: SourceFile, lineIdx: Int, columnIdx: Int): Position = {
    val lineOffset = source.lineToOffset(lineIdx)
    source.position(lineOffset + columnIdx)
  }

  private def filterMember(prefix: Option[String])(member: MemberInfo): Boolean = {
    lazy val startsWithPrefix = prefix.map(member.name.startsWith)
    !member.isConstructor && member.isAccessible && startsWithPrefix.getOrElse(true)
  }

  private def calculateMemberRank(prefix: Option[String])(member: MemberInfo): Int = {
    ((member.isLocal, 20) ::
    (member.isPublic, 5) ::
    (!member.isFromRootObjects, 10) ::
    (member.isInherited | false, 10) ::
    (prefix.isDefined, prefix.map(member.name.length - _.length) | 0) :: Nil)
      .foldLeft(100) {
        case (a ,(b, r)) if b => a + r
        case (a, _) => a
      }
  }

  def orderByRankDesc(prefix: Option[String]): scala.math.Ordering[MemberInfo]  =
    scala.math.Ordering.by(-calculateMemberRank(prefix)(_))

  def detectCompletionType(position: Position): CompletionType = {
    val lineContent = position.source.lineToString(position.line)
    detectCompletionType(lineContent, position.column - 1) // TODO: why -1
  }

  def members(completionType: CompletionType, position: Position, compiler: Compiler): Seq[MemberInfo] = {
    val memberInfoExtractor = extractMemberInfo(compiler) _
    completionType match {
      case Scope => compiler.typeCompletion(position, memberInfoExtractor)
      case Type =>
        val lastCharPosition = position.point - 1
        compiler.scopeCompletion(position.withPoint(lastCharPosition), memberInfoExtractor)
      case NoCompletion => Seq.empty
    }
  }

  def complete(sourceName: String, sourcePath: Path, lineIdx: Int, columnIdx: Int): Reader[Compiler, Seq[MemberInfo]] =
    for {
      source <- loadSource(sourceName, sourcePath)
      position = positionInSource(source, lineIdx, columnIdx)
      completionType = detectCompletionType(position)
      members <- Reader((c: Compiler) => members(completionType, position, c))
    } yield {
      members filter filterMember(None) sorted orderByRankDesc(None)
    }
}

trait ComplectionTypeDetection {
  sealed trait CompletionType
  case object Scope extends CompletionType
  case object Type extends CompletionType
  case object NoCompletion extends CompletionType

  def detectCompletionType(line: String, pos: Int): CompletionType
}

trait MemberInfoExtraction {
  def extractMemberInfo(global: Global)(member: global.Member): MemberInfo
}

trait DeclarationFinder {
  case class SymbolPosition(file: Path, lineIdx: Int, columnIdx: Int)

  def findSymbolDeclaration(sourcePath: Path, lineIdx: Int, columnIdx: Int): Reader[Compiler, SymbolPosition] =
    Reader { compiler =>
      val source = new BatchSourceFile(AbstractFile.getFile(sourcePath.toFile)) // TODO: dup
      // TODO: dup
      val lineOffset = source.lineToOffset(lineIdx)
      val positionToFind = source.position(lineOffset + columnIdx)
      //TODO: handle if not in source
      val foundPosition = compiler.findDeclarationOfSymbolAt(positionToFind)
      SymbolPosition(
        Paths.get(foundPosition.source.path),
        foundPosition.line,
        foundPosition.column
      )
    }
}

trait SourcesLoading { self: SourceFinderModule =>
  // side-effect
  def loadSourcesFrom(directories: Set[Path]): Reader[Compiler, Seq[SourceFile]] =
    Reader { compiler =>
      val sourcesPaths = findSourcesIn(directories).toList
      val sources = sourcesPaths.map { source =>
        //TODO: dup
        new BatchSourceFile(AbstractFile.getFile(source.toFile))
      }
      compiler.reloadSources(sources)
      sources
    }

  def loadSource(sourceName: String, sourcePath: Path): Reader[Compiler, SourceFile] =
    Reader { compiler =>
      val source = new BatchSourceFile(sourceName, AbstractFile.getFile(sourcePath.toFile).toCharArray)
      compiler.reloadSources(source :: Nil)
      source
    }
}

trait ImportsIndexModule { self: ClassFileIndexModule with SourceIndexModule =>
  case class ImportsIndex(sourceIndex: SourceIndex, classFileIndex: ClassFileIndex) {
    def lookupForClass(className: String): Set[String] =
      sourceIndex.lookup(className) ++ classFileIndex.lookup(className)
  }

  def createImportsIndex(classpath: Set[Path], sources: Seq[SourceFile]): Reader[Compiler, ImportsIndex] =
    Reader { compiler =>
      val classFileIndex = buildIndexFromClassFiles(classpath).run
      val sourcesIndex = createIndexForSources(sources).run(compiler)
      ImportsIndex(sourcesIndex, classFileIndex)
    }
}

trait Comp {
  def createCompiler(classpath: Set[Path]): Compiler =
    Compiler(classpath)
}

trait Configuraion {
  case class ProjectConfig(classpath: Set[Path], sourcesDirs: Set[Path])

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
