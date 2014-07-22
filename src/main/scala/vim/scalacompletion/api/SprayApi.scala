package vim.scalacompletion.api

import spray.routing.HttpService
import akka.pattern.ask
import akka.actor.{Actor, ActorRef, Props}
import vim.scalacompletion.{FacadeFactoryImpl, FacadeFactory,
                            MemberInfo, FacadeActor,
                            SourcesWatchActor, WatchService,
                            SourcesWatchActorFactory, ConfigLoader}
import FacadeActor._
import SourcesWatchActor._
import collection.JavaConversions._
import akka.util.Timeout
import scala.concurrent.duration._

class SprayApiActor extends Actor with SprayApi[MemberInfo] {
  var facade: ActorRef = _
  var sourcesWatcher: ActorRef = _
  val transformer = new VimFormatTransformer
  val facadeFactory: FacadeFactory[MemberInfo] = new FacadeFactoryImpl(context)
  val watchService: WatchService = new WatchService()

  val watchServiceThread = new Thread(watchService, "WatchService")
  watchServiceThread.setDaemon(true)
  watchServiceThread.start()

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)

  override def postStop() = {
    watchServiceThread.interrupt()
  }
}

trait SprayApi[T] extends HttpService {
  var facade: ActorRef
  val transformer: FormatTransformer[T]
  val facadeFactory: FacadeFactory[T]
  val watchService: WatchService

  implicit val timeout = Timeout(5.seconds)
  implicit def executionContext = actorRefFactory.dispatcher

  val apiRoutes = path("completion") {
    get {
      parameters('name, 'file_path, 'offset.as[Int], 'column.as[Int], 'prefix.?) { (name, filePath, offset, column, prefix) =>
        val future = (facade ? CompleteAt(name, filePath, offset, column, prefix)).mapTo[CompletionResult[T]].map { result =>
          transformer.transformCompletion(result.members)
        }
        complete(future)
      }
    }
  } ~
  path("init") {
    post {
      formField('conf) { configPath =>
        facade = facadeFactory.createFacade(watchService)
        //TODO: tests that it was ask, and not tell
        val future = (facade ? FacadeActor.Init(configPath)).map { _=> configPath }
        complete(future)
      }
    }
  }
}
