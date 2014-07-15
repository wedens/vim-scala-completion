package vim.scalacompletion.api

import org.specs2.mutable.Specification
import org.specs2.mock._
import spray.testkit.Specs2RouteTest
import spray.routing.HttpService
import spray.http.StatusCodes._
import java.net.URLEncoder
import vim.scalacompletion.CompilerApi

class SprayApiSpec extends Specification with Specs2RouteTest with SprayApi with Mockito  {
  def actorRefFactory = system
  val compiler = mock[CompilerApi]

  val code = """object app extends App {
    val str = "some string"
    println(str.)
  }
  """
  val urlEncodedCode = URLEncoder.encode(code, "UTF-8")

  "api" should {
    "return completion results for GET requests to /complete with code and completion position params" in {
      Get(s"/completion?line=2&column=14&body=$urlEncodedCode") ~> apiRoutes ~> check {
        // responseAs[String] must_== "[{'word': 'var1', 'menu': 'Int', info: 'Int'}]"
        responseAs[String] must_== code
      }
    }
  }
}
