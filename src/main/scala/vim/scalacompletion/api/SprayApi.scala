package vim.scalacompletion.api

import spray.routing.HttpService
import akka.pattern.ask
import akka.actor.{Actor, ActorRef, Props}
import vim.scalacompletion.{FacadeFactoryImpl, FacadeFactory,
                            MemberInfo, FacadeActor,
                            SourcesWatchActor, WatchService,
                            SourcesWatchActorFactory}
import FacadeActor._
import SourcesWatchActor._
import collection.JavaConversions._
import akka.util.Timeout
import scala.concurrent.duration._

class ConfigLoader {
  import java.io.{File => JFile}
  import com.typesafe.config.{Config, ConfigFactory}

  def load(path: String): Config = ConfigFactory.parseFile(new JFile(path))
}

class SprayApiActor extends Actor with SprayApi[MemberInfo] {
  var facade: ActorRef = _
  var sourcesWatcher: ActorRef = _
  val transformer = new VimFormatTransformer
  val facadeFactory: FacadeFactory[MemberInfo] = new FacadeFactoryImpl(context)
  val configLoader = new ConfigLoader
  val watchService: WatchService = new WatchService()
  val sourcesWatchActorFactory: SourcesWatchActorFactory = new SourcesWatchActorFactory(context)

  val watchServiceThread = new Thread(watchService, "WatchService")
  watchServiceThread.setDaemon(true)
  watchServiceThread.start()

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)

  override def postStop() = {
    watchServiceThread.interrupt()
  }
}

trait SprayApi[T] extends HttpService {
  var facade: ActorRef
  var sourcesWatcher: ActorRef
  val transformer: FormatTransformer[T]
  val facadeFactory: FacadeFactory[T]
  val configLoader: ConfigLoader
  val watchService: WatchService
  val sourcesWatchActorFactory: SourcesWatchActorFactory

  implicit val timeout = Timeout(5.seconds)
  implicit def executionContext = actorRefFactory.dispatcher

  val apiRoutes = path("completion") {
    get {
      parameters('name, 'file_path, 'offset.as[Int], 'column.as[Int], 'prefix.?) { (name, filePath, offset, column, prefix) =>
        val future = (facade ? CompleteAt(name, filePath, offset, column, prefix)).mapTo[CompletionResult[T]].map { result =>
          transformer.transformCompletion(result.members)
        }
        complete(future)
      }
    }
  } ~
  path("init") {
    post {
      formField('conf) { conf =>
        val config = configLoader.load(conf)
        val classpath = config.getStringList("vim.scala-completion.classpath")
        val sourceDirs = config.getStringList("vim.scala-completion.src-directories")
        facade = facadeFactory.createFacade(classpath)
        sourcesWatcher = sourcesWatchActorFactory.create(facade, watchService)
        facade ! ReloadSourcesInDirs(sourceDirs)
        sourcesWatcher ! WatchDirs(sourceDirs)
        complete(conf)
      }
    }
  }
}
