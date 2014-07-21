package vim.scalacompletion

import akka.testkit.{TestKit, TestActorRef}
import akka.actor._
import org.specs2.mutable._
import org.specs2.specification.BeforeExample
import org.specs2.mock._
import java.nio.file.Paths

class SourcesWatchActorSpec extends TestKit(ActorSystem("ComplexSupervisionTest")) with SpecificationLike with Mockito with BeforeExample {
  var watchServiceMock: WatchService = _
  var watchActor: TestActorRef[SourcesWatchActor] = _

  def before = {
    watchServiceMock = mock[WatchService]
    watchActor = TestActorRef(new SourcesWatchActor(watchServiceMock))
  }

  "sources watch actor" should {
    "register self as observer for changes" in {
      there was one(watchServiceMock).addObserver(watchActor)
    }

    "start watching dir on WatchDir message" in {
      val dir = Paths.get("/tmp")

      watchActor ! SourcesWatchActor.WatchDir(dir)

      there was one(watchServiceMock).watchRecursively(any)
    }

    "reload source when Created message received" in pending

    "reload source when Modifyed message received" in pending

    "remove source when Deleted message received" in pending
  }
}
