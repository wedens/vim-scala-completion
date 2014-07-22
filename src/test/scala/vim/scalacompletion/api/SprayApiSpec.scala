package vim.scalacompletion.api

import org.specs2.mutable.Specification
import org.specs2.mock._
import org.specs2.specification.BeforeExample
import org.specs2.time.NoTimeConversions
import org.mockito.Matchers.{eq => meq}
import spray.testkit.Specs2RouteTest
import akka.testkit.{TestProbe, TestActor}
import spray.routing.HttpService
import spray.http.StatusCodes._
import spray.http.FormData
import akka.actor.{ActorSystem, Actor, ActorRef}
import java.net.URLEncoder
import vim.scalacompletion.{FacadeActor, FacadeFactory}
import FacadeActor._

class SprayApiSpec extends Specification
                   with Specs2RouteTest
                   with SprayApi[String]
                   with Mockito
                   with BeforeExample
                   with NoTimeConversions {

  def actorRefFactory = system

  var facadeProbe: TestProbe = _
  val transformer = mock[FormatTransformer[String]]
  val facadeFactory = mock[FacadeFactory[String]]

  val path = "/src/main/scala/pkg/Source.scala"
  val tempPath = "/tmp/6157147744291722932"
  val urlEncodedName = URLEncoder.encode(path, "UTF-8")
  val urlEncodedFilePath = URLEncoder.encode(tempPath, "UTF-8")

  def before = {
     org.mockito.Mockito.reset(transformer)
     org.mockito.Mockito.reset(facadeFactory)

     facadeProbe = TestProbe()
     facade = facadeProbe.ref
     facadeProbe.setAutoPilot(new TestActor.AutoPilot {
       def run(sender: ActorRef, msg: Any): TestActor.AutoPilot =
         msg match {
           case _: CompleteAt =>
             sender ! CompletionResult(Seq[String]())
             TestActor.KeepRunning
           case _ : FacadeActor.Init =>
             sender ! FacadeActor.Initialized
             TestActor.KeepRunning
           case _ =>
             TestActor.KeepRunning
         }
     })

     facadeFactory.createFacade(any) returns facade
  }

  sequential

  "api" should {
    "GET /completion" should {
      def completionRequest(prefix: Option[String] = Some("abc")) = {
        val pfx = prefix.map(p => "&prefix=" + p) getOrElse ""
        Get(s"/completion?offset=25&column=14&name=$urlEncodedName&file_path=${urlEncodedFilePath}${pfx}") ~> apiRoutes
      }

      "call completion" in {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> check {
          facadeProbe.expectMsgType[CompleteAt]
          ok
        }
      }

      "call completion with correct position" in {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> check {
          facadeProbe.expectMsgType[CompleteAt] must beLike {
            case CompleteAt(_, _, 25, 14, _) => ok
          }
        }
      }

      "call completion with correct name" in {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> check {
          facadeProbe.expectMsgType[CompleteAt] must beLike {
            case CompleteAt(_name, _, _, _, _) => _name must_== path
          }
        }
      }

      "call completion with correct file path" in {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> check {
          facadeProbe.expectMsgType[CompleteAt] must beLike {
            case CompleteAt(_, _path, _, _, _) => _path must_== tempPath
          }
        }
      }

      "call completion with correct prefix" in {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> check {
          facadeProbe.expectMsgType[CompleteAt] must beLike {
            case CompleteAt(_, _, _, _, Some("abc")) => ok
          }
        }
      }

      "call completion without prefix" in {
        transformer.transformCompletion(any) returns ""
        completionRequest(prefix = None) ~> check {
          facadeProbe.expectMsgType[CompleteAt] must beLike {
            case CompleteAt(_, _, _, _, None) => ok
          }
        }
      }

      "transform completion result" in {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> check {
          there was one(transformer).transformCompletion(any)
        }
      }

      "complete request with transformed completion" in {
        val transformed = "[{'word': 'a'}, {'word': 'b'}]"
        transformer.transformCompletion(any) returns transformed

        completionRequest() ~> check {
          responseAs[String] must_== transformed
        }
      }
    }

    "POST /init" should {
      val configPath = "vim_scala_completion.conf"
      def initRequest = Post(s"/init", FormData(Map("conf" -> configPath))) ~> apiRoutes

      "initialize facade with config path" in {
        initRequest ~> check {
          facadeProbe.expectMsgType[FacadeActor.Init] must_== FacadeActor.Init(configPath)
        }
      }

      "complete with config path" in {
        initRequest ~> check {
          responseAs[String] must_== configPath
        }
      }
    }
  }
}
