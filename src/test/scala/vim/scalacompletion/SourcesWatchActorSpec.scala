package vim.scalacompletion

import akka.testkit._
import akka.actor._
import org.specs2.mutable._
import org.specs2.specification.BeforeExample
import org.specs2.mock._
import java.nio.file.Paths
import FacadeActor._

class SourcesWatchActorSpec extends TestKit(ActorSystem("ComplexSupervisionTest")) with SpecificationLike with Mockito with BeforeExample {

  var watchServiceMock: WatchService = _
  var watchActor: TestActorRef[SourcesWatchActor] = _
  var facadeProbe: TestProbe = _
  var facade: ActorRef = _
  var sourcesFinder: ScalaSourcesFinder = _

  val sourceFile = new java.io.File("/tmp/xxx.scala")
  val otherFile = new java.io.File("/tmp/xxx.cpp")

  def before = {
    watchServiceMock = mock[WatchService]
    sourcesFinder = mock[ScalaSourcesFinder]
    sourcesFinder.isScalaSource(sourceFile) returns true
    sourcesFinder.isScalaSource(otherFile) returns false
    facadeProbe = TestProbe()
    facade = facadeProbe.ref
    watchActor = TestActorRef(new SourcesWatchActor(facade, watchServiceMock, sourcesFinder))
  }

  sequential

  "sources watch actor" should {
    "register self as observer for changes" in {
      there was one(watchServiceMock).addObserver(watchActor)
    }

    "start watching dir on WatchDir message" in {
      val dir = Paths.get("/tmp")

      watchActor ! SourcesWatchActor.WatchDir(dir)

      there was one(watchServiceMock).watchRecursively(any)
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
