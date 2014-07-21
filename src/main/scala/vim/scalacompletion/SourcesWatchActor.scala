package vim.scalacompletion

import akka.actor._
import java.nio.file.Path

object SourcesWatchActor {
  case class WatchDir(dir: Path)
}

class SourcesWatchActor(watchService: WatchService) extends Actor {
  import SourcesWatchActor._
  import FileSystemEvents._

  watchService.addObserver(self)

  def receive = {
    case WatchDir(dir) =>
      watchService.watchRecursively(dir)
  }
}
