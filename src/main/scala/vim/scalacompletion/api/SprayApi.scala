package vim.scalacompletion.api

import spray.routing.HttpService
import vim.scalacompletion.CompilerApi

trait SprayApi extends HttpService {
  val compiler: CompilerApi

  val apiRoutes = path("completion") {
    get {
      parameters('line.as[Int], 'column.as[Int], 'body) { (line, column, body) =>
        complete(body)
      }
    }
  }

}
