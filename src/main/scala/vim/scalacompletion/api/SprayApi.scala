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

class SprayApiActor(override val transformer: FormatTransformer[MemberInfo],
                    override val facadeFactory: FacadeFactory[MemberInfo],
                    watchServiceThread: Thread) extends Actor with SprayApi[MemberInfo] {

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)

  // should be better place to do it...
  override def postStop() = {
    watchServiceThread.interrupt()
  }
}

trait SprayApi[T] extends HttpService {
  var facade: ActorRef = _
  val transformer: FormatTransformer[T]
  val facadeFactory: FacadeFactory[T]

  implicit val timeout = Timeout(5.seconds)
  implicit def executionContext = actorRefFactory.dispatcher

  val apiRoutes = path("completion") {
    get {
      parameters('name, 'file_path, 'offset.as[Int], 'prefix.?) { (name, filePath, offset, prefix) =>
        val future = (facade ? CompleteAt(name, filePath, offset, prefix)).mapTo[CompletionResult[T]].map { result =>
          transformer.transformCompletion(result.members)
        }
        complete(future)
      }
    }
  } ~
  path("init") {
    post {
      formField('conf) { configPath =>
        facade = facadeFactory.createFacade(actorRefFactory)
        //TODO: tests that it was ask, and not tell
        val future = (facade ? FacadeActor.Init(configPath)).map { _=> configPath }
        complete(future)
      }
    }
  }
}
