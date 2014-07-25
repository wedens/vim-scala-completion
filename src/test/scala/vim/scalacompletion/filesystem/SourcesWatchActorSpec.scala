package vim.scalacompletion.filesystem

import akka.actor._
import akka.testkit._
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.BeforeExample
import vim.scalacompletion.FacadeActor.{ReloadSources, RemoveSources}

class SourcesWatchActorSpec extends TestKit(ActorSystem("ComplexSupervisionTest"))
                            with ImplicitSender
                            with SpecificationLike
                            with Mockito
                            with BeforeExample {

  var watchService: WatchService = _
  var watchActor: TestActorRef[SourcesWatchActor] = _
  var facadeProbe: TestProbe = _
  var facade: ActorRef = _
  var sourcesFinder: ScalaSourcesFinder = _

  val sourceFile = mock[java.io.File]
  val otherFile = mock[java.io.File]
  val dirs = Seq("/tmp", "/var")

  def before = {
    watchService = mock[WatchService]
    sourcesFinder = mock[ScalaSourcesFinder]
    sourcesFinder.isScalaSource(sourceFile) returns true
    sourcesFinder.isScalaSource(otherFile) returns false
    facadeProbe = TestProbe()
    facade = facadeProbe.ref
    watchActor = TestActorRef(new SourcesWatchActor(facade, watchService, sourcesFinder))
  }

  sequential

  "sources watch actor" should {
    "register self as observer for changes" in {
      there was one(watchService).addObserver(watchActor)
    }

    "start watching two dirs on WatchDirs message" in {
      watchActor ! SourcesWatchActor.WatchDirs(dirs)

      there was two(watchService).watchRecursively(any)
    }

    "reply with Watching message" in {
      watchActor ! SourcesWatchActor.WatchDirs(dirs)

      expectMsgType[SourcesWatchActor.Watching] must_== SourcesWatchActor.Watching(dirs)
    }

    "reload source when Created message received if file is scala source" in {
       watchActor ! FileSystemEvents.Created(sourceFile)

       facadeProbe.expectMsgType[ReloadSources] must_== ReloadSources(Seq(sourceFile))
       ok
    }

    "not reload file when Created message received and file is not scala source" in {
       watchActor ! FileSystemEvents.Created(otherFile)

       facadeProbe.expectNoMsg()
       ok
    }

    "reload source when Modifyed message received if file is scala source" in {
      watchActor ! FileSystemEvents.Modifyed(sourceFile)

      facadeProbe.expectMsgType[ReloadSources] must_== ReloadSources(Seq(sourceFile))
      ok
    }

    "not reload file when Modifyed message received and file is not scala source" in {
       watchActor ! FileSystemEvents.Modifyed(otherFile)

       facadeProbe.expectNoMsg()
       ok
    }

    "remove source when Deleted message received if file is scala source" in {
      watchActor ! FileSystemEvents.Deleted(sourceFile)

      facadeProbe.expectMsgType[RemoveSources] must_== RemoveSources(Seq(sourceFile))
      ok
    }

    "not remove file when Deleted message received and file is not scala source" in {
      watchActor ! FileSystemEvents.Deleted(otherFile)

      facadeProbe.expectNoMsg()
      ok
    }
  }
}
