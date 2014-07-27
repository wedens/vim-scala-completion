package vim.scalacompletion.filesystem

import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileSystems, FileVisitResult, Files, Path, SimpleFileVisitor}

import akka.actor.ActorRef
import vim.scalacompletion.WithLog
import vim.scalacompletion.filesystem.FileSystemEvents._

import scala.collection.JavaConversions._


class WatchService extends Runnable with WithLog {
  private val watchService = FileSystems.getDefault.newWatchService()
  private var observerActors: Seq[ActorRef] = Seq.empty

  def addObserver(actor: ActorRef): Unit = observerActors = observerActors :+ actor

  def watchRecursively(root: Path) {
    watch(root)
    Files.walkFileTree(root, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
        watch(dir)
        FileVisitResult.CONTINUE
      }
    })
  }

  private def notify(msg: FileSystemEvent): Unit = {
    observerActors.foreach(_ ! msg)
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
            case ENTRY_CREATE => notify(Created(pathAsFile))
            case ENTRY_DELETE => notify(Deleted(pathAsFile))
            case ENTRY_MODIFY => notify(Modified(pathAsFile))
            case x => logg.warn(s"Unknown event kind: $x at path $path")
          }
        }
      }
    } catch {
      case e: InterruptedException => logg.info("Watch service is stopped.")
    } finally {
      watchService.close()
    }
  }
}
