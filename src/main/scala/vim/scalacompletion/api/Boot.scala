package vim.scalacompletion.api

import akka.actor.{ActorSystem, Props, Actor, Terminated}
import akka.pattern.ask
import akka.io.IO
import spray.can.Http
import vim.scalacompletion.WithLog
import akka.util.Timeout
import vim.scalacompletion.filesystem._
import vim.scalacompletion.compiler._
import vim.scalacompletion.completion._
import vim.scalacompletion._
import scala.concurrent.duration._

object Boot extends App with WithLog {
  logg.info("Starting application...")

  lazy val watchService             = new WatchService
  lazy val scalaSourcesFinder       = new ScalaSourcesFinder
  lazy val sourcesWatchActorFactory = new SourcesWatchActorFactory(
                                            scalaSourcesFinder,
                                            watchService)

  lazy val completionTypeDetector   = new CompletionTypeDetector
  lazy val extractor                = new MemberInfoExtractorForMemberInfo
  lazy val membersFilter            = MemberInfoFilter
  lazy val memberRankCalculator     = MemberRankCalculatorImpl
  lazy val completionHandlerFactory = new CompletionHandlerFactory(
                                            completionTypeDetector,
                                            extractor,
                                            membersFilter,
                                            memberRankCalculator)

  lazy val configLoader             = new ConfigLoader
  lazy val sourceFileFactory        = new SourceFileFactoryImpl
  lazy val compilerFactory          = new CompilerFactoryImpl
  lazy val facadeFactory            = new FacadeFactory(
                                            configLoader,
                                            sourceFileFactory,
                                            scalaSourcesFinder,
                                            sourcesWatchActorFactory,
                                            compilerFactory,
                                            completionHandlerFactory)

  lazy val transformer              = new VimFormatTransformer()

  lazy val watchServiceThread       = new Thread(watchService, "WatchService")

  implicit val system = ActorSystem("vim-scalacompletion")
  val api = system.actorOf(Props(new SprayApiActor(transformer, facadeFactory)), "api")

  val apiWatcher = system.actorOf(Props(new Actor {
    context.watch(api)
    def receive = {
      case Terminated(api) => watchServiceThread.interrupt()
    }
  }))

  implicit val bindingTimeout = Timeout(1.second)
  import system.dispatcher
  val port = 8085
  val boundFuture = IO(Http) ? Http.Bind(api, "localhost", port = port)
  boundFuture onSuccess {
    case _: Http.Bound =>
      watchServiceThread.setDaemon(true)
      watchServiceThread.start()
      logg.info("Scala completion server started.")
    case Http.CommandFailed(_: Http.Bind) =>
      logg.error(s"Unable to start scala completion server. Port $port is occupied by some other process.")
      system.shutdown()
  }
}
