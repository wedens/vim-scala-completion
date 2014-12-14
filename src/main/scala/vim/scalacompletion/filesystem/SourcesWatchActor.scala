//package vim.scalacompletion.filesystem
//
//import java.nio.file.Paths
//import akka.actor.{Actor, ActorContext, ActorRef, Props}
//
//class SourcesWatchActorFactory(scalaSourcesFinder: ScalaSourcesFinder,
//                               watchService: WatchService) {
//  def create(project: ActorRef)(implicit context: ActorContext) = {
//    context.actorOf(Props(new SourcesWatchActor(project, watchService, scalaSourcesFinder)))
//  }
//}
//
//object SourcesWatchActor {
//  case class WatchDirs(dirs: Seq[String])
//  case class Watching(dirs: Seq[String])
//}
//
//class SourcesWatchActor(projectActor: ActorRef,
//                        watchService: WatchService,
//                        scalaSourcesFinder: ScalaSourcesFinder) extends Actor {
//  import vim.scalacompletion.filesystem.FileSystemEvents._
//  import vim.scalacompletion.filesystem.SourcesWatchActor._
//  import vim.scalacompletion.Project._
//
//  watchService.addObserver(self)
//
//  def receive = {
//    case WatchDirs(dirs) =>
//      dirs.foreach { dir =>
//        val path = Paths.get(dir)
//        watchService.watchRecursively(path)
//      }
//      sender ! Watching(dirs)
//    case Created(file) if scalaSourcesFinder.isScalaSource(file) =>
//      projectActor ! ReloadSources(Seq(file))
//    case Modified(file) if scalaSourcesFinder.isScalaSource(file) =>
//      projectActor ! ReloadSources(Seq(file))
//    case Deleted(file) if scalaSourcesFinder.isScalaSource(file) =>
//      projectActor ! RemoveSources(Seq(file))
//  }
//}
