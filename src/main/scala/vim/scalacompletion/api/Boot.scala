package vim.scalacompletion.api

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.io.IO
import spray.can.Http
import vim.scalacompletion.WithLog
import akka.util.Timeout
import scala.concurrent.duration._
import vim.scalacompletion.{FacadeFactoryImpl, WatchService}

object Boot extends App with WithLog {
  logg.info("Starting application...")
  implicit val system = ActorSystem("vim-scalacompletion")

  val watchService: WatchService = new WatchService()
  val watchServiceThread = new Thread(watchService, "WatchService")
  watchServiceThread.setDaemon(true)
  watchServiceThread.start() //stopped in api actor

  val facadeFactory = new FacadeFactoryImpl(watchService)

  val transformer = new VimFormatTransformer()

  val api = system.actorOf(Props(new SprayApiActor(transformer, facadeFactory, watchServiceThread)), "api")

  implicit val bindingTimeout = Timeout(1.second)
  import system.dispatcher
  val boundFuture = IO(Http) ? Http.Bind(api, "localhost", port = 8085)
  boundFuture.foreach { _ => logg.info("Application started") }
}
