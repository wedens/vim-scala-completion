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

  //TODO: move timeouts to one place
  implicit val timeout = Timeout(25.seconds)
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
          projects.ask(Projects.Create(configPath))(30.seconds).map { _=> configPath }
        }
      }
    }
  } ~
  path("imports") {
    get {
      parameters('name, 'class_name) { (name, className) =>
        complete {
          for {
            project <- projects.ask(Projects.GetProjectFor(name)).mapTo[ActorRef]
            packages <- project.ask(LookupPackagesForClass(className)).mapTo[Set[String]]
          } yield transformer.transformImportSuggestions(className, packages)
        }
      }
    }
  } ~
  path("declaration") {
    get {
      parameters('name, 'file_path, 'line.as[Int], 'column.as[Int]) {
        (name, filePath, lineIdx, columnIdx) =>
          complete {
            for {
              project <- projects.ask(Projects.GetProjectFor(name)).mapTo[ActorRef]
              pos <- project.ask(FindDeclaration(name, filePath, lineIdx, columnIdx))
            } yield pos.toString
          }
      }
    }
  } ~
  path("package"){
    get {
      parameter('name) { name =>
        complete {
          for {
            project <- projects.ask(Projects.GetProjectFor(name)).mapTo[ActorRef]
            pkg <- project.ask(GetPackage(name)).mapTo[Option[String]]
          } yield pkg
        }
      }
    }
  }
}
