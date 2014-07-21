package vim.scalacompletion

import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._
import collection.JavaConversions._
import java.nio.file.WatchEvent.Modifier
import akka.actor.ActorRef
import FileSystemEvents._

class SourcesWatchService(observerActor: ActorRef) extends Runnable with WithLog {
  private val watchService = FileSystems.getDefault.newWatchService()

  def watchRecursively(root: Path) {
    watch(root)
    Files.walkFileTree(root, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
        watch(dir)
        FileVisitResult.CONTINUE
      }
    })
  }

  private def watch(path: Path) =
    path.register(watchService, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)

  override def run(): Unit = {
    try {
      while (!Thread.currentThread.isInterrupted) {
        val key = watchService.take()
        key.pollEvents().foreach { event =>
          val relativePath = event.context().asInstanceOf[Path]
          val path = key.watchable().asInstanceOf[Path].resolve(relativePath)
          val pathAsFile = path.toFile
          event.kind() match {
            case ENTRY_CREATE if pathAsFile.isDirectory => watchRecursively(path)
            case ENTRY_CREATE => observerActor ! Created(pathAsFile)
            case ENTRY_DELETE => observerActor ! Deleted(pathAsFile)
            case ENTRY_MODIFY => observerActor ! Modifyed(pathAsFile)
            case x => logg.warn(s"Unknown event kind: $x at path $path")
          }
        }
      }
    } catch {
      case e: InterruptedException => logg.info("Source watch service is stopped.")
    } finally {
      watchService.close()
    }
  }
}
