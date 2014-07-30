package vim.scalacompletion.api

import spray.routing.HttpService
import akka.pattern.ask
import akka.actor.{Actor, ActorRef}
import vim.scalacompletion.compiler.MemberInfo
import vim.scalacompletion.filesystem.SourcesWatchActor
import vim.scalacompletion.FacadeActor._
import vim.scalacompletion.Projects
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
        val future = for {
          facade           <- (projects ? Projects.GetFacadeFor(name)).mapTo[ActorRef]
          completionResult <- (facade ? CompleteAt(name, filePath, offset, prefix))
                                                           .mapTo[CompletionResult[T]]
        } yield transformer.transformCompletion(completionResult.members)

        complete(future)
      }
    }
  } ~
  path("init") {
    post {
      formField('conf) { configPath =>
        val future = (projects ? Projects.Create(configPath)).map { _=> configPath }
        complete(future)
      }
    }
  }
}
