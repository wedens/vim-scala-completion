package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import org.specs2.time.NoTimeConversions
import org.specs2.specification.Scope
import org.specs2.matcher.ThrownExpectations
import scala.concurrent.duration._
import scala.reflect.internal.util.Position
import scala.tools.nsc.interactive.Global
import scala.reflect.internal.util.SourceFile
import scala.concurrent.duration._
// import org.specs2.specification.BeforeExample
import org.mockito.Matchers.{eq => meq}
import java.io.{File => JFile}
import akka.actor._
import akka.pattern.ask
import akka.testkit._
import akka.util.Timeout
import scala.util.{Try, Success, Failure}
import com.typesafe.config.Config
import collection.JavaConversions._
import FacadeActor._
import scala.reflect.internal.util.Position

trait createdFacade extends Scope
             with Mockito
             with ThrownExpectations { outerSelf =>

  implicit val system: ActorSystem

  var compilerMock = mock[Compiler]
  var sourceFileFactory = mock[SourceFileFactory]
  var scalaSourcesFinder = mock[ScalaSourcesFinder]
  var configLoader = mock[ConfigLoader]
  var compilerFactory = mock[CompilerFactory]
  var sourcesWatchActorFactory = mock[SourcesWatchActorFactory]
  var watchService = mock[WatchService]
  val completionHandlerFactory = mock[CompletionHandlerFactory[String]]
  val completionHandler = mock[CompletionHandler[String]]

  val facade = TestActorRef(new FacadeActor[String] {
    compiler = outerSelf.compilerMock
    completionHandler = outerSelf.completionHandler
    val sourceFileFactory = outerSelf.sourceFileFactory
    val scalaSourcesFinder = outerSelf.scalaSourcesFinder
    val compilerFactory = outerSelf.compilerFactory
    val configLoader = outerSelf.configLoader
    val sourcesWatchActorFactory = outerSelf.sourcesWatchActorFactory
    val watchService = outerSelf.watchService
    val completionHandlerFactory = outerSelf.completionHandlerFactory
  })
}

class facadeInit(override implicit val system: ActorSystem) extends facadeSources {
  val sourceDir1 = mock[JFile]
  val sourceDir2 = mock[JFile]
  val sourceDirs = List(sourceDir1, sourceDir2)

  scalaSourcesFinder.findIn(any) returns sourceFiles

  val configPath = "/tmp/xxx.conf"
  val config = mock[Config]
  val sourceDirsStr = Seq("/tmp", "/opt")
  config.getStringList("vim.scala-completion.src-directories") returns sourceDirsStr
  configLoader.load(configPath) returns config

  val sourcesWatcherProbe = TestProbe()
  sourcesWatcherProbe.setAutoPilot(new TestActor.AutoPilot {
    def run(sender: ActorRef, msg: Any): TestActor.AutoPilot =
      msg match {
        case SourcesWatchActor.WatchDirs(d) =>
          sender ! SourcesWatchActor.Watching(d)
          TestActor.KeepRunning
      }
  })
  val sourcesWatcher = sourcesWatcherProbe.ref
  sourcesWatchActorFactory.create(facade) returns sourcesWatcher

  completionHandlerFactory.create(compilerMock) returns completionHandler
  compilerFactory.create(any) returns compilerMock
}

class facadeCompletion(override implicit val system: ActorSystem) extends createdFacade {
  val source = mock[SourceFile]
  val position = mock[Position]
  val offset = 35
  val sourceName = "/src/main/scala/pkg/Source.scala"
  val sourcePath = "/tmp/6157147744291722932"
  val completionResult = Seq("map", "toInt", "reduce")
  completionHandler.complete(meq(position), any) returns completionResult
  sourceFileFactory.createSourceFile(sourceName, sourcePath) returns source
  source.position(offset) returns position
}


class facadeSources(override implicit val system: ActorSystem) extends createdFacade {
  val sourceFile1 = mock[JFile]
  val sourceFile2 = mock[JFile]
  val sourceFiles = List(sourceFile1, sourceFile2)

  val sourceFile1Path = "/tmp/file1.scala"
  val sourceFile2Path = "/opt/file2.scala"
  sourceFile1.getCanonicalPath returns sourceFile1Path
  sourceFile2.getCanonicalPath returns sourceFile2Path

  val source = mock[SourceFile]
  sourceFileFactory.createSourceFile(sourceFile1Path) returns source
  sourceFileFactory.createSourceFile(sourceFile2Path) returns source
}

class FacadeActorSpec extends TestKit(ActorSystem("FacadeActorSpec"))
                      with ImplicitSender
                      with SpecificationLike
                      with NoTimeConversions {

  implicit val timeout = Timeout(5.seconds)

  "facade" should {
    "initialization" should {
      val configPath = "/tmp/xxx.conf" //TODO

      "reload sources in path from config" in new facadeInit {
        facade ! Init(configPath)

        there was one(compilerMock).reloadSources(any)
      }

      "start watching source dirs from config" in new facadeInit {
        facade ! Init(configPath)

        sourcesWatcherProbe.expectMsgType[SourcesWatchActor.WatchDirs] must_== SourcesWatchActor.WatchDirs(sourceDirsStr)
      }

      "respond with Initialized message" in new facadeInit {
        val future = facade ? Init(configPath)

        val Success(result: Initialized.type) = future.value.get
        ok
      }
    }

    "completion" should {
      //TODO
      val offset = 35
      val sourceName = "/src/main/scala/pkg/Source.scala"
      val sourcePath = "/tmp/6157147744291722932"
      def completeAt = CompleteAt(sourceName, sourcePath, offset, None)

      "respond with completion result" in new facadeCompletion {
        val future = (facade ? completeAt).mapTo[CompletionResult[String]]

        val Success(result: CompletionResult[String]) = future.value.get
        result.members must_== completionResult
      }

      "reload source before triggering completion" in new facadeCompletion {
        facade ! completeAt

        there was one(compilerMock).reloadSources(List(source))
      }

      "call completion at position from source" in new facadeCompletion {
        facade ! completeAt

        there was one(completionHandler).complete(position, None)
      }
    }

    "reloading source files" should {
      "ask compiler to reload sources" in new facadeSources {
        facade ! ReloadSources(Seq())

        there was one(compilerMock).reloadSources(any)
      }
    }

    "removing source files" should {
      "ask compiler to remove sources" in new facadeSources {
        facade ! RemoveSources(Seq())

        there was one(compilerMock).removeSources(any)
      }
    }
  }
}
