package vim.scalacompletion.api

import spray.routing.HttpService
import akka.pattern.ask
import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import vim.scalacompletion.completion.MemberInfo
import scala.concurrent.duration._
import vim.scalacompletion.Projects._
import vim.scalacompletion.PositionInSource
import java.nio.file.Paths


class SprayApiActor(override val transformer: VimFormatTransformer,
                    override val projects: ActorRef
                    ) extends Actor with SprayApi[MemberInfo] {

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)
}

trait SprayApi[T] extends HttpService {
  val transformer: VimFormatTransformer
  val projects: ActorRef

  //TODO: move timeouts to one place
  implicit val timeout = Timeout(25.seconds)
  implicit def executionContext = actorRefFactory.dispatcher

  val apiRoutes =
    (path("completion") & get) {
      parameters('name, 'file_path, 'line.as[Int], 'column.as[Int], 'prefix.?) {
        (name, filePath, lineIdx, columnIdx, prefix) =>
          complete {
            val position = PositionInSource(name, Paths.get(filePath),
              lineIdx, columnIdx )
            projects.ask(Complete(position, prefix))
              .mapTo[Seq[MemberInfo]]
              .map(transformer.transformCompletion)
          }
      }
    } ~
    path("init") {
      post {
        formField('file_path) { filePath =>
          complete {
            projects.ask(InitProject(Paths.get(filePath)))(30.seconds).map {
              case ex: Throwable => throw ex
              case _ => "Project loaded"
            }
          }
        }
      }
    } ~
    (path("imports") & get) {
      parameters('file_path, 'class_name) { (filePath, className) =>
        complete {
          projects.ask(SuggestImportsForClass(Paths.get(filePath), className))
            .mapTo[Set[String]]
            .map(transformer.transformImportSuggestions(className, _))
        }
      }
    } ~
    (path("declaration") & get) {
      parameters('name, 'file_path, 'line.as[Int], 'column.as[Int]) {
        (name, filePath, lineIdx, columnIdx) =>
          complete {
            val position = PositionInSource(Paths.get(filePath), lineIdx,
              columnIdx)
            projects.ask(FindDeclaration(position)).map(_.toString)
          }
      }
    } ~
    (path("package") & get) {
      parameter('file_path) { filePath =>
        complete {
          projects.ask(CalculatePackageForFile(Paths.get(filePath))).mapTo[Option[String]]
        }
      }
    }
}
