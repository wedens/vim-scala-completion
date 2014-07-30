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
  val projectProbe = TestProbe()
  val projectActor = projectProbe.ref
  val projectFactory = mock[ProjectFactory[Any]]
  val projects = TestActorRef(new Projects(projectFactory))
  projectFactory.createProject(any) returns projectActor

  val projectPathStr = "/tmp/dir"
  val projectPath = Paths.get(projectPathStr)
  val configPathStr = "/tmp/dir/config.conf"
}

class existingProject(override implicit val system: ActorSystem) extends init {
  val fileName = Paths.get("/tmp/dir/file.scala")
  val configPath = Paths.get(configPathStr)
  val projectInfo1 = mock[Projects.ProjectInfo]
  val projectInfo2 = mock[Projects.ProjectInfo]
  projectInfo1.contains(fileName) returns false
  projectInfo2.contains(fileName) returns true
  projectInfo2.projectActor returns projectActor
  projectInfo2.configPath returns configPath
  projects.underlyingActor.projects = Map((Paths.get("/opt") -> projectInfo1),
                                          (projectPath -> projectInfo2))
}

class ProjectsSpec extends TestKit(ActorSystem("ProjectsSpec"))
                      with ImplicitSender
                      with SpecificationLike
                      with NoTimeConversions {

  "projects" should {
    "obtaining project by path" should {
      "respond with project" in new existingProject {
        projects ! Projects.GetProjectFor(fileName.toString)

        expectMsgType[ActorRef] must_== projectActor
      }
    }

    "project creation" should {
      "create new project" in new init {
        projects ! Projects.Create(configPathStr)

        projects.underlyingActor.projects must have size 1
      }

      "initialize project" in new init {
        projects ! Projects.Create(configPathStr)

        projectProbe.expectMsgType[Project.Init] must_== Project.Init(configPathStr)
      }
    }
  }
}

