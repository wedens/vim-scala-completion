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

  var compilerApi: Compiler = _
  var completionTypeDetector: CompletionTypeDetector = _
  var sourceFileFactory: SourceFileFactory = _
  var membersFilter: MemberFilter[String] = _
  var memberRankCalculator: MemberRankCalculator[String] = _
  var scalaSourcesFinder: ScalaSourcesFinder = _
  var configLoader: ConfigLoader = _
  var compilerFactory: CompilerFactory = _
  var sourcesWatchActorFactory: SourcesWatchActorFactory = _
  var watchService: WatchService = _
  var sourcesWatcherProbe: TestProbe = _
  var sourcesWatcher: ActorRef = _
  var config: Config = _
  var memberInfoExtractorFactory: MemberInfoExtractorFactory = _

  var facade: TestActorRef[FacadeActor[String]] = _

  implicit val timeout = Timeout(5 seconds)
  val sourceName = "/src/main/scala/pkg/Source.scala"
  val sourcePath = "/tmp/6157147744291722932"
  val offset = 35
  val column = 15
  var file1Mock: JFile = _
  var file2Mock: JFile = _
  var files: Seq[JFile] = _
  val configPath = "/tmp/xxx.conf"
  val classpath = List("lib1.jar", "/tmp/lib2.jar")
  val sourcesDirs = List("/tmp", "/opt")

  def before = {
    file1Mock = mock[JFile]
    file1Mock.getCanonicalPath returns "/tmp/file1.scala"
    file2Mock = mock[JFile]
    file2Mock.getCanonicalPath returns "/opt/file2.scala"
    files = Seq(file1Mock, file2Mock)

    config = mock[Config]
    config.getStringList("vim.scala-completion.classpath") returns classpath
    config.getStringList("vim.scala-completion.src-directories") returns sourcesDirs

    configLoader = mock[ConfigLoader]
    configLoader.load(any) returns config


    watchService = mock[WatchService]
    memberInfoExtractorFactory = mock[MemberInfoExtractorFactory]

    compilerApi = mock[Compiler]
    compilerApi.typeCompletion(any, any) returns List()
    compilerApi.scopeCompletion(any, any) returns List()

    compilerFactory = mock[CompilerFactory]
    compilerFactory.create(any) returns compilerApi

    completionTypeDetector = mock[CompletionTypeDetector]
    sourceFileFactory = mock[SourceFileFactory]
    sourceFileFactory.createSourceFile(anyString, anyString) returns mock[SourceFile]

    scalaSourcesFinder = mock[ScalaSourcesFinder]
    scalaSourcesFinder.findIn(any) returns files

    membersFilter = mock[MemberFilter[String]]
    membersFilter.apply(any, any) returns true

    memberRankCalculator = mock[MemberRankCalculator[String]]
    memberRankCalculator.apply(any, any) returns 0

    sourcesWatcherProbe = TestProbe()
    sourcesWatcher = sourcesWatcherProbe.ref

    sourcesWatchActorFactory = mock[SourcesWatchActorFactory]
    sourcesWatchActorFactory.create(any, any) returns sourcesWatcher

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

        there was one(sourcesWatchActorFactory).create(facade, watchService)
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

      "detect completion type" in {
        stubSourceFactory()
        completionTypeDetector.detect(anyString, anyInt) returns CompletionType.NoCompletion

        facade ! CompleteAt(sourceName, sourcePath, offset, column, Some(""))

        there was one(completionTypeDetector).detect(anyString, anyInt)
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
