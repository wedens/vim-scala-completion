package vim.scalacompletion

import java.io.{File => JFile}
import akka.actor.{ActorRefFactory, Props, ActorRef}

trait FacadeFactory[T] {
  def createFacade(actorRefFactory: ActorRefFactory): ActorRef
}

class FacadeFactoryImpl(watchSrv: WatchService) extends FacadeFactory[MemberInfo] {
  def createFacade(actorRefFactory: ActorRefFactory): ActorRef = {
    actorRefFactory.actorOf(Props(new FacadeActorImpl(watchSrv)))
  }
}

