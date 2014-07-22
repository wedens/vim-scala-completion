package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import org.specs2.time.NoTimeConversions
import scala.tools.nsc.interactive.Global
import scala.reflect.internal.util.SourceFile
import scala.concurrent.duration._
import org.specs2.specification.BeforeExample
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


class FacadeActorSpec extends TestKit(ActorSystem("FacadeSpec"))
                      with SpecificationLike
                      with Mockito
                      with BeforeExample
                      with NoTimeConversions { selfSpec =>

  var compilerApi = mock[Compiler]
  var completionTypeDetector = mock[CompletionTypeDetector]
  var sourceFileFactory = mock[SourceFileFactory]
  var membersFilter = mock[MemberFilter[String]]
  var memberRankCalculator = mock[MemberRankCalculator[String]]
  var scalaSourcesFinder = mock[ScalaSourcesFinder]
  var configLoader = mock[ConfigLoader]
  var compilerFactory = mock[CompilerFactory]
  var sourcesWatchActorFactory = mock[SourcesWatchActorFactory]
  var watchService = mock[WatchService]
  var sourcesWatcherProbe = mock[TestProbe]
  var sourcesWatcher = mock[ActorRef]
  var config = mock[Config]
  var memberInfoExtractorFactory = mock[MemberInfoExtractorFactory[String]]

  var facade = mock[TestActorRef[FacadeActor[String]]]

  implicit val timeout = Timeout(5 seconds)
  val sourceName = "/src/main/scala/pkg/Source.scala"
  val sourcePath = "/tmp/6157147744291722932"
  val offset = 35
  val column = 15
  var file1Mock = mock[JFile]
  var file2Mock = mock[JFile]
  var files = Seq(file1Mock, file2Mock)
  val configPath = "/tmp/xxx.conf"
  val classpath = List("lib1.jar", "/tmp/lib2.jar")
  val sourcesDirs = List("/tmp", "/opt")

  def before = {
    org.mockito.Mockito.reset(config)
    org.mockito.Mockito.reset(file1Mock)
    org.mockito.Mockito.reset(file2Mock)
    org.mockito.Mockito.reset(configLoader)
    org.mockito.Mockito.reset(watchService)
    org.mockito.Mockito.reset(memberInfoExtractorFactory)
    org.mockito.Mockito.reset(compilerApi)
    org.mockito.Mockito.reset(compilerFactory)
    org.mockito.Mockito.reset(completionTypeDetector)
    org.mockito.Mockito.reset(sourceFileFactory)
    org.mockito.Mockito.reset(scalaSourcesFinder)
    org.mockito.Mockito.reset(memberRankCalculator)
    org.mockito.Mockito.reset(membersFilter)
    org.mockito.Mockito.reset(sourcesWatchActorFactory)

    file1Mock.getCanonicalPath returns "/tmp/file1.scala"
    file2Mock.getCanonicalPath returns "/opt/file2.scala"

    config.getStringList("vim.scala-completion.classpath") returns classpath
    config.getStringList("vim.scala-completion.src-directories") returns sourcesDirs

    configLoader.load(any) returns config

    compilerApi.typeCompletion(any, any) returns List()
    compilerApi.scopeCompletion(any, any) returns List()

    compilerFactory.create(any) returns compilerApi

    sourceFileFactory.createSourceFile(anyString, anyString) returns mock[SourceFile]

    scalaSourcesFinder.findIn(any) returns files

    membersFilter.apply(any, any) returns true

    memberRankCalculator.apply(any, any) returns 0

    sourcesWatcherProbe = TestProbe()
    sourcesWatcherProbe.setAutoPilot(new TestActor.AutoPilot {
       def run(sender: ActorRef, msg: Any): TestActor.AutoPilot =
         msg match {
           case SourcesWatchActor.WatchDirs(d) =>
             sender ! SourcesWatchActor.Watching(d)
             TestActor.KeepRunning
           case _ =>
             TestActor.KeepRunning
         }
     })
    sourcesWatcher = sourcesWatcherProbe.ref

    sourcesWatchActorFactory.create(any) returns sourcesWatcher

    facade = TestActorRef(new FacadeActor[String] {
      compilerApi = selfSpec.compilerApi
      val completionTypeDetector = selfSpec.completionTypeDetector
      extractor = (m: Compiler#Member) => m.toString
      val sourceFileFactory = selfSpec.sourceFileFactory
      val membersFilter = selfSpec.membersFilter
      val memberRankCalculator = selfSpec.memberRankCalculator
      val scalaSourcesFinder = selfSpec.scalaSourcesFinder
      val compilerFactory = selfSpec.compilerFactory
      val configLoader = selfSpec.configLoader
      val sourcesWatchActorFactory = selfSpec.sourcesWatchActorFactory
      val memberInfoExtractorFactory = selfSpec.memberInfoExtractorFactory
      val watchService = selfSpec.watchService
    })
  }

  sequential

  "facade" should {
    "initialization" should {
      "load config from provided path" in {
        facade ! Init(configPath)

        there was one(configLoader).load(configPath)
      }

      "create compiler with classpah from config" in {
        facade ! Init(configPath)

        there was one(compilerFactory).create(classpath.map(new JFile(_)))
      }

      "create sources watcher with sources path from config" in {
        facade ! Init(configPath)

        there was one(sourcesWatchActorFactory).create(facade)
      }

      "reload sources in path from config" in {
        facade ! Init(configPath)

        there was one(compilerApi).reloadSources(any)
      }

      "start watching source dirs from config" in {
        facade ! Init(configPath)

        sourcesWatcherProbe.expectMsgType[SourcesWatchActor.WatchDirs] must_== SourcesWatchActor.WatchDirs(sourcesDirs)
      }

      "respond with Initialized message" in {
        val future = facade ? Init(configPath)

        val Success(result) = future.mapTo[Initialized.type].value.get
        ok
      }
    }

    "completion" should {
      "update source" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(compilerApi).reloadSources(any[List[SourceFile]])
      }

      "call type completion when detector says type" in  {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(compilerApi).typeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "call scope completion when detector says scope" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Scope

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(compilerApi).scopeCompletion(any[scala.reflect.internal.util.Position], any)
      }

      "not call any completion when detector says no completion" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(compilerApi).reloadSources(any)
        there were noMoreCallsTo(compilerApi)
      }

      "return empty seq when no completion" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        val future = facade ? CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        val Success(result: CompletionResult[String]) = future.mapTo[CompletionResult[String]].value.get
        result.members must be empty
      }

      "call completion type detector with correct parameters" in {
        stubSourceFactory(line = "abc123")

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(completionTypeDetector).detect("abc123", 15)
      }

      "create source with correct parameters" in {
        stubSourceFactory()

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(sourceFileFactory).createSourceFile(sourceName, sourcePath)
      }

      "get position equal to offset for scope completion" in {
        val source = stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Scope

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(source).position(offset)
      }

      "get position with offset before dot or space for type completion" in {
        val source = stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(source).position(offset - 1)
      }

      "filter members" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some("pfx"))

        there was two(membersFilter).apply(Some("pfx"), "str")
      }

      "rank members" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(memberRankCalculator).apply(any, meq("str"))
      }

      "rank members with prefix" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str")

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some("pfx"))

        there was one(memberRankCalculator).apply(meq(Some("pfx")), any)
      }

      "sort members by rank desc" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns Seq("str", "str2")
        memberRankCalculator.apply(any, anyString) returns 1 thenReturns 10

        val future = facade ? CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        val Success(result: CompletionResult[String]) = future.mapTo[CompletionResult[String]].value.get
        result.members must_== Seq("str2", "str")
      }

      "limit result by 15" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.Type
        compilerApi.typeCompletion[String](any, any) returns (1 to 15).map(_.toString)

        val future = facade ? CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        val Success(result: CompletionResult[String]) = future.mapTo[CompletionResult[String]].value.get
        result.members must have size(15)
      }
    }

    "reloading all sources in directories" should {
      "find sources in directories" in {
        facade ! ReloadSourcesInDirs(sourcesDirs)

        there was one(scalaSourcesFinder).findIn(List(new JFile("/tmp"), new JFile("/opt")))
      }

      "create compiler's source files for found sources" in {
        facade ! ReloadSourcesInDirs(sourcesDirs)

        there was one(sourceFileFactory).createSourceFile("/tmp/file1.scala") andThen one(sourceFileFactory).createSourceFile("/opt/file2.scala")
      }

      "ask compiler to reload sources" in {
        facade ! ReloadSourcesInDirs(sourcesDirs)

        there was one(compilerApi).reloadSources(any)
      }
    }

    "reloading source files" should {
      "ask compiler to reload sources" in {
        facade ! ReloadSources(Seq())

        there was one(compilerApi).reloadSources(any)
      }
    }

    "removing source files" should {
      "ask compiler to remove sources" in {
        facade ! RemoveSources(Seq())

        there was one(compilerApi).removeSources(any)
      }
    }
  }

  def stubSourceFactory(lineIdx: Int = 0, line: String = "") = {
    val mockSourceFile = mock[SourceFile]
    mockSourceFile.lineToString(anyInt) returns line
    mockSourceFile.offsetToLine(anyInt) returns lineIdx
    mockSourceFile.position(anyInt) returns mock[scala.reflect.internal.util.Position]
    sourceFileFactory.createSourceFile(anyString, anyString) returns mockSourceFile
    mockSourceFile
  }
}
