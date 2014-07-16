package vim.scalacompletion.api

import org.specs2.mutable.Specification
import org.specs2.mock._
import spray.testkit.Specs2RouteTest
import spray.routing.HttpService
import spray.http.StatusCodes._
import java.net.URLEncoder
import vim.scalacompletion.Facade
import org.specs2.specification.BeforeExample
import org.mockito.Matchers.{eq => meq}

class SprayApiSpec extends Specification with Specs2RouteTest
  with SprayApi with Mockito {

  def actorRefFactory = system
  val facade = mock[Facade]
  val transformer = mock[FormatTransformer[facade.MemberInfoType]]

  val path = "/src/main/scala/pkg/Source.scala"
  val tempPath = "/tmp/6157147744291722932"
  val urlEncodedName = URLEncoder.encode(path, "UTF-8")
  val urlEncodedFilePath = URLEncoder.encode(tempPath, "UTF-8")

  isolated

  "api" should {
    "GET /completion" should {
      "call completion" in {
        transformer.transformCompletion(any) returns ""
        Get(s"/completion?offset=25&column=14&name=$urlEncodedName&file_path=$urlEncodedFilePath") ~> apiRoutes ~> check {
          there was one(facade).completeAt(anyString, anyString, anyInt, anyInt)
        }
      }

      "call completion with correct position" in {
        transformer.transformCompletion(any) returns ""
        Get(s"/completion?offset=25&column=14&name=$urlEncodedName&file_path=$urlEncodedFilePath") ~> apiRoutes ~> check {
          there was one(facade).completeAt(anyString, anyString,  meq(25), meq(14))
        }
      }

      "call completion with correct name" in {
        transformer.transformCompletion(any) returns ""
        Get(s"/completion?offset=25&column=14&name=$urlEncodedName&file_path=$urlEncodedFilePath") ~> apiRoutes ~> check {
          there was one(facade).completeAt(meq(path), anyString, anyInt, anyInt)
        }
      }

      "call completion with correct file path" in {
        transformer.transformCompletion(any) returns ""
        Get(s"/completion?offset=25&column=14&name=$urlEncodedName&file_path=$urlEncodedFilePath") ~> apiRoutes ~> check {
          there was one(facade).completeAt(anyString, meq(tempPath), anyInt, anyInt)
        }
      }

      "transform completion result" in {
        transformer.transformCompletion(any) returns ""
        Get(s"/completion?offset=25&column=14&name=$urlEncodedName&file_path=$urlEncodedFilePath") ~> apiRoutes ~> check {
          there was one(transformer).transformCompletion(any)
        }
      }

      "complete request with transformed completion" in {
        val transformed = "[{'word': 'a'}, {'word': 'b'}]"
        transformer.transformCompletion(any) returns transformed

        Get(s"/completion?offset=25&column=14&name=$urlEncodedName&file_path=$urlEncodedFilePath") ~> apiRoutes ~> check {
          responseAs[String] must_== transformed
        }
      }
    }
  }
}
