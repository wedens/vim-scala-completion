package vim.scalacompletion.api

import spray.routing.HttpService
import akka.actor.Actor
import vim.scalacompletion.{FacadeFactoryImpl, FacadeFactory, MemberInfo, Facade}
import collection.JavaConversions._

class ConfigLoader {
  import java.io.{File => JFile}
  import com.typesafe.config.{Config, ConfigFactory}

  def load(path: String): Config = ConfigFactory.parseFile(new JFile(path))
}

class SprayApiActor extends Actor with SprayApi[MemberInfo] {
  var facade: Facade[MemberInfo] = _
  val transformer = new VimFormatTransformer
  val facadeFactory: FacadeFactory[MemberInfo] = FacadeFactoryImpl
  val configLoader = new ConfigLoader

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)
}

trait SprayApi[T] extends HttpService {
  var facade: Facade[T]
  val transformer: FormatTransformer[T]
  val facadeFactory: FacadeFactory[T]
  val configLoader: ConfigLoader

  val apiRoutes = path("completion") {
    get {
      parameters('name, 'file_path, 'offset.as[Int], 'column.as[Int], 'prefix.?) { (name, filePath, offset, column, prefix) =>
        val completionResult = facade.completeAt(name, filePath, offset, column, prefix)
        val transformedCompletion = transformer.transformCompletion(completionResult)
        complete(transformedCompletion)
      }
    }
  } ~
  path("init") {
    post {
      formField('conf) { conf =>
        val config = configLoader.load(conf)
        val classpath = config.getStringList("vim.scala-completion.classpath")
        val sourceDirs = config.getStringList("vim.scala-completion.src-directories").toList
        facade = facadeFactory.createFacade(classpath)
        facade.reloadAllSourcesInDirs(sourceDirs)
        complete(conf)
      }
    }
  }
}
