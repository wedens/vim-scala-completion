package vim.scalacompletion

import java.io.{File => JFile}
import akka.actor.{ActorRefFactory, Props, ActorRef}

trait FacadeFactory[T] {
  def createFacade(watchService: WatchService): ActorRef
}

class FacadeFactoryImpl(actorRefFactory: ActorRefFactory) extends FacadeFactory[MemberInfo] {
  def createFacade(watchSrv: WatchService): ActorRef = {
    actorRefFactory.actorOf(Props(new FacadeActor[MemberInfo] {
      val compilerFactory = new CompilerFactoryImpl()
      val memberInfoExtractorFactory = new MemberInfoExtractorFactory()
      val completionTypeDetector = new CompletionTypeDetector
      val sourceFileFactory = new SourceFileFactoryImpl
      val membersFilter: MemberFilter[MemberInfo] = MemberInfoFilter
      val memberRankCalculator: MemberRankCalculator[MemberInfo] = MemberRankCalculatorImpl
      val scalaSourcesFinder = new ScalaSourcesFinder
      val configLoader = new ConfigLoader()
      val sourcesWatchActorFactory = new SourcesWatchActorFactory(context)
      val watchService = watchSrv
    }))
  }
}
