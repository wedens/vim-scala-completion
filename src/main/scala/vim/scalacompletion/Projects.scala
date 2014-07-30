package vim.scalacompletion

import akka.actor.{SupervisorStrategy, Actor,
                   ActorLogging, ActorRef,
                   OneForOneStrategy, Terminated}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scala.concurrent.duration._
import java.nio.file.{Path, Paths}

object Projects {
  case class ProjectInfo(configPath: Path, projectPath: Path, projectActor: ActorRef) {
    def contains(filePath: Path) = filePath.startsWith(projectPath)
  }

  case class Create(configPath: String)
  case class GetProjectFor(filePath: String)
}

class Projects[T](projectFactory: ProjectFactory[T])
                                  extends Actor with ActorLogging {
  import Project._
  import Projects._

  implicit val timeout = Timeout(5.seconds)
  implicit val ec = context.dispatcher

  override val supervisorStrategy = OneForOneStrategy() {
    case _: Exception => SupervisorStrategy.Restart
  }

  var projects = Map[Path, ProjectInfo]()

  def receive = {
    case GetProjectFor(filePathStr) =>
      val filePath = Paths.get(filePathStr)
      val project = getProjectFor(filePath).map(_.projectActor).get
      //TODO: handle not found
      sender ! project
    case Create(configPathStr) =>
      val configPath = Paths.get(configPathStr)
      val projectPath = configPath.getParent
      val projectActor = projectFactory.createProject(context)
      projects = projects + (projectPath -> ProjectInfo(configPath, projectPath, projectActor))
      (projectActor ? Init(configPathStr)) pipeTo sender
  }

  def getProjectFor(filePath: Path) =
    projects.find { case (_, project) => project.contains(filePath) }.map(_._2)
}
