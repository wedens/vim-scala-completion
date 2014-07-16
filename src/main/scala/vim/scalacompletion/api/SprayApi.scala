package vim.scalacompletion.api

import spray.routing.HttpService
import vim.scalacompletion.{Facade, MemberInfoExtractor,
                    CompilerFactory, SourceFileFactoryImpl, MemberInfo,
                    CompletionTypeDetector, MemberInfoFilter}
import akka.actor.Actor
import akka.actor.ActorLogging
import java.io.{File => JFile}

class SprayApiActor extends Actor with SprayApi with ActorLogging {
  val facade = new Facade {
    type MemberInfoType = MemberInfo
    val compilerApi = CompilerFactory(Seq(
      new JFile("/home/wedens/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.1.jar"),
      new JFile("/usr/lib/jvm/java-7-oracle/jre/lib/rt.jar"),
      new JFile("/home/wedens/.ivy2/cache/org.scalaz/scalaz-core_2.11/jars/scalaz-core_2.11-7.0.6.jar")
    ))
    val extractor = MemberInfoExtractor(compilerApi)
    val completionTypeDetector = new CompletionTypeDetector
    val sourceFileFactory = new SourceFileFactoryImpl
    val membersFilter: MemberInfoType => Boolean = MemberInfoFilter
  }

  val transformer = new VimFormatTransformer

  def actorRefFactory = context
  def receive = runRoute(apiRoutes)
}

trait SprayApi extends HttpService {
  val facade: Facade
  val transformer: FormatTransformer[facade.MemberInfoType]

  val apiRoutes = path("completion") {
    get {
      parameters('name, 'file_path, 'offset.as[Int], 'column.as[Int], 'prefix) { (name, filePath, offset, column, prefix) =>
        val completionResult = facade.completeAt(name, filePath, offset, column, prefix)
        val transformedCompletion = transformer.transformCompletion(completionResult)
        complete(transformedCompletion)
      }
    }
  }

}
