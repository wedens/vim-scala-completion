package vim.scalacompletion.api

import akka.actor.{ActorSystem, Props}
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
  implicit val system = ActorSystem("vim-scalacompletion")

  val watchService: WatchService = new WatchService()
  val watchServiceThread = new Thread(watchService, "WatchService")
  watchServiceThread.setDaemon(true)
  //TODO: start after successful initialization
  watchServiceThread.start() //stopped in api actor

  val compilerFactory = new CompilerFactoryImpl
  val sourceFileFactory = new SourceFileFactoryImpl
  val scalaSourcesFinder = new ScalaSourcesFinder
  val configLoader = new ConfigLoader
  val sourcesWatchActorFactory = new SourcesWatchActorFactory(scalaSourcesFinder,
                                                              watchService)

  val completionTypeDetector = new CompletionTypeDetector
  val membersFilter = MemberInfoFilter
  val memberRankCalculator = MemberRankCalculatorImpl
  val extractor = new MemberInfoExtractorForMemberInfo
  val completionHandlerFactory = new CompletionHandlerFactory(completionTypeDetector,
                                                              extractor,
                                                              membersFilter,
                                                              memberRankCalculator)

  val facadeFactory = new FacadeFactoryImpl(compilerFactory,
                                            sourceFileFactory,
                                            scalaSourcesFinder,
                                            configLoader,
                                            sourcesWatchActorFactory,
                                            completionHandlerFactory)

  val transformer = new VimFormatTransformer()

  val api = system.actorOf(Props(new SprayApiActor(transformer, facadeFactory, watchServiceThread)), "api")

  implicit val bindingTimeout = Timeout(1.second)
  import system.dispatcher
  val port = 8085
  val boundFuture = IO(Http) ? Http.Bind(api, "localhost", port = port)
  boundFuture onSuccess {
    case _: Http.Bound => logg.info("Scala completion server started.")
    case Http.CommandFailed(_: Http.Bind) =>
      logg.error(s"Unable to start scala completion server. Port $port is occupied by some other process.")
      system.shutdown()
  }
}
