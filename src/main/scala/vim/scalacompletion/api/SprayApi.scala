package vim.scalacompletion.api

import spray.routing.HttpService
import akka.pattern.ask
import akka.actor.{Actor, ActorRef}
import vim.scalacompletion.compiler.MemberInfo
import vim.scalacompletion.filesystem.SourcesWatchActor
import vim.scalacompletion.Project._
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
      parameters('name, 'file_path, 'line.as[Int], 'column.as[Int], 'prefix.?) { (name, filePath, lineIdx, columnIdx, prefix) =>
        complete {
          val completeAtMsg = CompleteAt(name, filePath, lineIdx, columnIdx, prefix)
          for {
            project          <- (projects ? Projects.GetProjectFor(name)).mapTo[ActorRef]
            completionResult <- (project ? completeAtMsg)
              .mapTo[CompletionResult[T]]
          } yield transformer.transformCompletion(completionResult.members)
        }
      }
    }
  } ~
  path("init") {
    post {
      formField('conf) { configPath =>
        complete {
          (projects ? Projects.Create(configPath)).map { _=> configPath }
        }
      }
    }
  }
}
