package vim.scalacompletion.api

import spray.routing.HttpService
import akka.pattern.ask
import akka.actor.{Actor, ActorRef, Props}
import vim.scalacompletion.{FacadeFactoryImpl, FacadeFactory, MemberInfo, FacadeActor}
import FacadeActor._
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
  val transformer = new VimFormatTransformer
  val facadeFactory: FacadeFactory[MemberInfo] = new FacadeFactoryImpl(context)
  val configLoader = new ConfigLoader

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)
}

trait SprayApi[T] extends HttpService {
  var facade: ActorRef
  val transformer: FormatTransformer[T]
  val facadeFactory: FacadeFactory[T]
  val configLoader: ConfigLoader

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
        facade ! ReloadSourcesInDirs(sourceDirs)
        complete(conf)
      }
    }
  }
}
