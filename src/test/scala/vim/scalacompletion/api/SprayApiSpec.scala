package vim.scalacompletion.api

import org.specs2.mutable.Specification
import org.specs2.mock._
import org.specs2.specification.BeforeExample
import org.specs2.time.NoTimeConversions
import org.specs2.specification.Scope
import org.specs2.matcher.ThrownExpectations
import org.mockito.Matchers.{eq => meq}
import akka.testkit.{TestProbe, TestActor}
import akka.actor.{ActorSystem, Actor, ActorRef}
import spray.testkit.Specs2RouteTest
import spray.routing.HttpService
import spray.http.StatusCodes._
import spray.http.FormData
import java.net.URLEncoder

import vim.scalacompletion.{Project, ProjectFactory}
import vim.scalacompletion.Projects
import vim.scalacompletion.Project._

class apiSetup(implicit sys: ActorSystem) extends Scope
                                          with Mockito
                                          with ThrownExpectations
                                          with SprayApi[String] {
  override def actorRefFactory = sys
  val projectProbe = TestProbe()
  projectProbe.setAutoPilot(new TestActor.AutoPilot {
    def run(sender: ActorRef, msg: Any): TestActor.AutoPilot =
      msg match {
        case _: CompleteAt =>
          sender ! CompletionResult(Seq[String]())
          TestActor.KeepRunning
     }
  })
  val project = projectProbe.ref
  val projectsProbe = TestProbe()
  projectsProbe.setAutoPilot(new TestActor.AutoPilot {
    def run(sender: ActorRef, msg: Any): TestActor.AutoPilot =
      msg match {
        case _: Projects.GetProjectFor =>
          sender ! project
          TestActor.KeepRunning
        case _: Projects.Create =>
          sender ! Project.Initialized
          TestActor.KeepRunning
     }
  })
  val projects = projectsProbe.ref
  val transformer = mock[FormatTransformer[String]]
}

class SprayApiSpec extends Specification
                   with Specs2RouteTest
                   with NoTimeConversions {

  val sourcePath = "/src/main/scala/pkg/Source.scala"
  val tempPath = "/tmp/6157147744291722932"
  val urlEncodedName = URLEncoder.encode(sourcePath, "UTF-8")
  val urlEncodedFilePath = URLEncoder.encode(tempPath, "UTF-8")

  sequential

  "api" should {
    "GET /completion" should {
      def completionRequest(prefix: Option[String] = Some("abc")) = {
        val pfx = prefix.map(p => "&prefix=" + p) getOrElse ""
        Get(s"/completion?offset=25&name=$urlEncodedName&file_path=${urlEncodedFilePath}${pfx}")
      }

      "call completion with correct position" in new apiSetup {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> apiRoutes ~> check {
          projectProbe.expectMsgType[CompleteAt].offset must_== 25
        }
      }

      "call completion with correct name" in new apiSetup {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> apiRoutes ~> check {
          projectProbe.expectMsgType[CompleteAt].name must_== sourcePath
        }
      }

      "call completion with correct file path" in new apiSetup {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> apiRoutes ~> check {
          projectProbe.expectMsgType[CompleteAt].path must_== tempPath
        }
      }

      "call completion with correct prefix" in new apiSetup {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> apiRoutes ~> check {
          projectProbe.expectMsgType[CompleteAt].prefix must beSome("abc")
        }
      }

      "call completion without prefix" in new apiSetup {
        transformer.transformCompletion(any) returns ""
        completionRequest(prefix = None) ~> apiRoutes ~> check {
          projectProbe.expectMsgType[CompleteAt].prefix must beNone
        }
      }

      "transform completion result" in new apiSetup {
        transformer.transformCompletion(any) returns ""
        completionRequest() ~> apiRoutes ~> check {
          there was one(transformer).transformCompletion(any)
        }
      }

      "complete request with transformed completion" in new apiSetup {
        val transformed = "[{'word': 'a'}, {'word': 'b'}]"
        transformer.transformCompletion(any) returns transformed

        completionRequest() ~> apiRoutes ~> check {
          responseAs[String] must_== transformed
        }
      }
    }

    "POST /init" should {
      val configPath = "vim_scala_completion.conf"
      def initRequest = Post(s"/init", FormData(Map("conf" -> configPath)))

      "initialize project with config path" in new apiSetup {
        initRequest ~> apiRoutes ~> check {
          projectsProbe.expectMsgType[Projects.Create] must_== Projects.Create(configPath)
        }
      }

      "complete with config path" in new apiSetup {
        initRequest ~> apiRoutes ~> check {
          responseAs[String] must_== configPath
        }
      }
    }
  }
}
