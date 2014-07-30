package vim.scalacompletion

import akka.actor.{SupervisorStrategy, Actor,
                   ActorLogging, ActorRef,
                   OneForOneStrategy}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scala.concurrent.duration._
import java.nio.file.{Path, Paths}

object Projects {
  case class ProjectInfo(configPath: Path, projectPath: Path, facade: ActorRef) {
    def contains(filePath: Path) = filePath.startsWith(projectPath)
  }

  case class Create(configPath: String)
}

class Projects[T](facadeFactory: FacadeFactory[T])
                                  extends Actor with ActorLogging {
  import FacadeActor._
  import Projects._

  implicit val timeout = Timeout(5.seconds)
  implicit val ec = context.dispatcher

  override val supervisorStrategy = OneForOneStrategy() {
    case _: Exception => SupervisorStrategy.Restart
  }

  var projects = Map[Path, ProjectInfo]()

  def receive = {
    case Create(configPathStr) =>
      val configPath = Paths.get(configPathStr)
      val projectPath = configPath.getParent
      val facade = facadeFactory.createFacade(context)
      projects = projects + (projectPath -> ProjectInfo(configPath, projectPath, facade))
      (facade ? Init(configPathStr)) pipeTo sender
    case msg: CompleteAt =>
      val filePath = Paths.get(msg.name)
      getProjectFor(filePath) match {
        case Some(project) => project.facade forward msg
        // TODO: send failure to sender
        case None => log.warning(s"Project is not started!")
      }
  }

  def getProjectFor(filePath: Path) =
    projects.find { case (_, project) => project.contains(filePath) }.map(_._2)
}
