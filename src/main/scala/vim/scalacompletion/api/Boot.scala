package vim.scalacompletion.api

import akka.actor.{ActorSystem, Props, Actor, Terminated}
import akka.pattern.ask
import akka.io.IO
import spray.can.Http
import vim.scalacompletion.WithLog
import vim.scalacompletion.Projects
import akka.util.Timeout
import scala.concurrent.duration._

object Boot extends App with WithLog {
  implicit val system = ActorSystem("vim-scalacompletion")
  implicit val bindingTimeout: Timeout = Timeout(5.second)

  val transformer = new VimFormatTransformer
  val projects = system.actorOf(Props[Projects], "Projects")
  val api = system.actorOf(Props(new SprayApiActor(transformer, projects)), "api")

  import system.dispatcher
  val port = 8085
  val boundFuture = IO(Http) ? Http.Bind(api, "localhost", port = port)
  boundFuture onSuccess {
    case _: Http.Bound =>
      // watchServiceThread.setDaemon(true)
      // watchServiceThread.start()
      logg.info("Scala completion server started.")
    case Http.CommandFailed(_: Http.Bind) =>
      logg.error(s"Unable to start scala completion server. Port $port is occupied by some other process.")
      system.shutdown()
  }

  // system.registerOnTermination {
  //   watchServiceThread.interrupt()
  // }
}
