package vim.scalacompletion

import akka.actor.{ActorRefFactory, Props, ActorRef}
import vim.scalacompletion.compiler.MemberInfo
import vim.scalacompletion.filesystem.WatchService

trait FacadeFactory[T] {
  def createFacade(actorRefFactory: ActorRefFactory): ActorRef
}

class FacadeFactoryImpl(watchSrv: WatchService) extends FacadeFactory[MemberInfo] {
  def createFacade(actorRefFactory: ActorRefFactory): ActorRef = {
    actorRefFactory.actorOf(Props(new FacadeActorImpl(watchSrv)))
  }
}

