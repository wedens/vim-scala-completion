package vim.scalacompletion.api

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.io.IO
import spray.can.Http
import vim.scalacompletion.WithLog
import akka.util.Timeout
import scala.concurrent.duration._

object Boot extends App with WithLog {
  logg.info("Starting application...")
  implicit val system = ActorSystem("vim-scalacompletion")
  val service = system.actorOf(Props[SprayApiActor], "api")

  implicit val bindingTimeout = Timeout(1.second)
  import system.dispatcher // execution context for the future
  val boundFuture = IO(Http) ? Http.Bind(service, "localhost", port = 8085)
  boundFuture.foreach { _ => logg.info("Application started") }
}
