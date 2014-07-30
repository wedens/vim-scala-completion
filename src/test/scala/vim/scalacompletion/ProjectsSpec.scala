package vim.scalacompletion

import org.specs2.mutable._
import org.specs2.mock._
import org.specs2.time.NoTimeConversions
import org.specs2.specification.Scope
import org.specs2.matcher.ThrownExpectations
// import org.mockito.Matchers.{eq => meq}
import akka.actor._
import akka.testkit._
import java.nio.file.{Path, Paths}

class init(implicit val system: ActorSystem) extends Scope
                                                 with Mockito
                                                 with ThrownExpectations {
  val facadeProbe = TestProbe()
  val facade = facadeProbe.ref
  val facadeFactory = mock[FacadeFactory[Any]]
  val projects = TestActorRef(new Projects(facadeFactory))
  facadeFactory.createFacade(any) returns facade


  val projectPathStr = "/tmp/dir"
  val projectPath = Paths.get(projectPathStr)
  val configPathStr = "/tmp/dir/config.conf"
}

class existingProject(override implicit val system: ActorSystem) extends init {
  val fileName = Paths.get("/tmp/dir/file.scala")
  val project1 = mock[Projects.ProjectInfo]
  val project2 = mock[Projects.ProjectInfo]
  project1.contains(fileName) returns false
  project2.contains(fileName) returns true
  project2.facade returns facade
  projects.underlyingActor.projects = Map((Paths.get("/opt") -> project1),
                                          (projectPath -> project2))
}

class ProjectsSpec extends TestKit(ActorSystem("ProjectsSpec"))
                      with ImplicitSender
                      with SpecificationLike
                      with NoTimeConversions {

  "projects" should {
    "obtaining facade by path" should {
      "respond with facade" in new existingProject {
        projects ! Projects.GetFacadeFor(fileName.toString)

        expectMsgType[ActorRef] must_== facade
      }
    }

    "project creation" should {
      "create new project" in new init {
        projects ! Projects.Create(configPathStr)

        projects.underlyingActor.projects must have size 1
      }

      "initialize project" in new init {
        projects ! Projects.Create(configPathStr)

        facadeProbe.expectMsgType[FacadeActor.Init] must_== FacadeActor.Init(configPathStr)
      }
    }
  }
}

