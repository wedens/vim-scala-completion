package vim.scalacompletion

import java.io.{File => JFile}
import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import vim.scalacompletion.compiler._
import vim.scalacompletion.completion._
import vim.scalacompletion.filesystem.{ScalaSourcesFinder, WatchService, SourcesWatchActor, SourcesWatchActorFactory}
import scala.concurrent.duration._
import collection.JavaConversions._

object FacadeActor {
  case class CompleteAt(name: String, path: String,
    offset: Int, prefix: Option[String])
  case class CompletionResult[T](members: Seq[T])

  case class ReloadSources(sources: Seq[JFile])
  case class RemoveSources(sources: Seq[JFile])
  case class Init(configPath: String)
  case object Initialized
}

class FacadeActorImpl(
  override val compilerFactory: CompilerFactory,
  override val sourceFileFactory: SourceFileFactory,
  override val scalaSourcesFinder: ScalaSourcesFinder,
  override val configLoader: ConfigLoader,
  override val sourcesWatchActorFactory: SourcesWatchActorFactory,
  override val completionHandlerFactory: CompletionHandlerFactory[MemberInfo]
) extends FacadeActor[MemberInfo]

trait FacadeActor[MemberInfoType] extends Actor with WithLog {
  import FacadeActor._

  val configLoader: ConfigLoader
  val sourceFileFactory: SourceFileFactory
  val scalaSourcesFinder: ScalaSourcesFinder
  val sourcesWatchActorFactory: SourcesWatchActorFactory
  val compilerFactory: CompilerFactory
  val completionHandlerFactory: CompletionHandlerFactory[MemberInfoType]

  var compiler: Compiler = _
  var sourcesWatcher: ActorRef = _
  var completionHandler: CompletionHandler[MemberInfoType] = _

  implicit val timeout = Timeout(5.seconds)
  implicit val ec = context.dispatcher

  def receive = {
    case Init(configPath)                       => init(configPath)
    case CompleteAt(name, path, offset, prefix) => completeAt(name, path, offset, prefix)
    case ReloadSources(sources)                 => reloadSources(sources)
    case RemoveSources(sources)                 => removeSources(sources)
  }

  def completeAt(name: String, path: String, offset: Int, prefix: Option[String]) = {
    val source = sourceFileFactory.createSourceFile(name, path)
    compiler.reloadSources(List(source))

    val position = source.position(offset)
    val members = completionHandler.complete(position, prefix, Some(50))
    sender ! CompletionResult(members)
  }

  def init(configPath: String) = {
    val config = configLoader.load(configPath) // TODO: not exists?
    val classpath = config.getStringList("vim.scala-completion.classpath")
    val sourcesDirs = config.getStringList("vim.scala-completion.src-directories")

    compiler = compilerFactory.create(classpath.map(new JFile(_)))
    sourcesWatcher = sourcesWatchActorFactory.create(self)
    completionHandler = completionHandlerFactory.create(compiler)
    reloadAllSourcesInDirs(sourcesDirs)

    val originalSender = sender
    (sourcesWatcher ? SourcesWatchActor.WatchDirs(sourcesDirs)).foreach { _ =>
      originalSender ! Initialized
    }
  }

  def reloadSources(sourcesJFiles: Seq[JFile]) = {
    val sources = filesToSourceFiles(sourcesJFiles)
    compiler.reloadSources(sources)
  }

  def removeSources(sourcesJFiles: Seq[JFile]) = {
    val sources = filesToSourceFiles(sourcesJFiles)
    compiler.removeSources(sources)
  }

  private def reloadAllSourcesInDirs(dirs: Seq[String]) = {
    val sourcesJFiles = scalaSourcesFinder.findIn(dirs.map(new JFile(_)).toList)
    reloadSources(sourcesJFiles)
  }

  private def filesToSourceFiles(sourcesJFiles: Seq[JFile]) = {
    sourcesJFiles.map { file =>
      val canonicalPath = file.getCanonicalPath
      sourceFileFactory.createSourceFile(canonicalPath)
    }.toList
  }
}
