package vim.scalacompletion.api

import spray.routing.HttpService
import akka.pattern.ask
import akka.actor.{Actor, ActorRef}
import vim.scalacompletion.compiler.MemberInfo
import vim.scalacompletion.filesystem.SourcesWatchActor
import vim.scalacompletion.{FacadeFactory, FacadeActor}
import FacadeActor._
import akka.util.Timeout
import scala.concurrent.duration._

class SprayApiActor(override val transformer: FormatTransformer[MemberInfo],
                    override val projects: ActorRef
                    ) extends Actor with SprayApi[MemberInfo] {

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)
}

trait SprayApi[T] extends HttpService {
  val transformer: FormatTransformer[T]
  val projects: ActorRef

  implicit val timeout = Timeout(5.seconds)
  implicit def executionContext = actorRefFactory.dispatcher

  val apiRoutes = path("completion") {
    get {
      parameters('name, 'file_path, 'offset.as[Int], 'prefix.?) { (name, filePath, offset, prefix) =>
        val future = (projects ? CompleteAt(name, filePath, offset, prefix))
                    .mapTo[CompletionResult[T]].map { result =>
          transformer.transformCompletion(result.members)
        }
        complete(future)
      }
    }
  } ~
  path("init") {
    post {
      formField('conf) { configPath =>
        //TODO: tests that it was ask, and not tell
        val future = (projects ? FacadeActor.Init(configPath)).map { _=> configPath }
        complete(future)
      }
    }
  }
}
