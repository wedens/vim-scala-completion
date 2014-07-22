package vim.scalacompletion

import akka.actor.{Actor, ActorRef}
import java.nio.file.Path

object SourcesWatchActor {
  case class WatchDir(dir: Path)
}

class SourcesWatchActor(facadeActor: ActorRef,
                        watchService: WatchService,
                        scalaSourcesFinder: ScalaSourcesFinder) extends Actor {
  import SourcesWatchActor._
  import FileSystemEvents._
  import FacadeActor._

  watchService.addObserver(self)

  def receive = {
    case WatchDir(dir) =>
      watchService.watchRecursively(dir)
    case Created(file) if scalaSourcesFinder.isScalaSource(file) =>
      facadeActor ! ReloadSources(Seq(file))
    case Modifyed(file) if scalaSourcesFinder.isScalaSource(file) =>
      facadeActor ! ReloadSources(Seq(file))
    case Deleted(file) if scalaSourcesFinder.isScalaSource(file) =>
      facadeActor ! RemoveSources(Seq(file))
  }
}
