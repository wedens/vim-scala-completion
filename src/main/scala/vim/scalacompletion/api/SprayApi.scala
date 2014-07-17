package vim.scalacompletion.api

import spray.routing.HttpService
import akka.actor.Actor
import vim.scalacompletion.{FacadeFactoryImpl, FacadeFactory, MemberInfo, Facade}

class SprayApiActor extends Actor with SprayApi[MemberInfo] {
  var facade: Facade[MemberInfo] = _
  val transformer = new VimFormatTransformer
  val facadeFactory: FacadeFactory[MemberInfo] = FacadeFactoryImpl

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)
}

trait SprayApi[T] extends HttpService {
  var facade: Facade[T]
  val transformer: FormatTransformer[T]
  val facadeFactory: FacadeFactory[T]

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
        facade = facadeFactory.createFacade(Seq.empty)
        complete(conf)
      }
    }
  }

}
