package vim.scalacompletion

import akka.actor.{ActorRefFactory, Props, ActorRef}
import vim.scalacompletion.compiler._
import vim.scalacompletion.filesystem._
import vim.scalacompletion.completion._

class ProjectFactory[T](
    configLoader: ConfigLoader,
    sourceFileFactory: SourceFileFactory,
    scalaSourcesFinder: ScalaSourcesFinder,
    sourcesWatchActorFactory: SourcesWatchActorFactory,
    compilerFactory: CompilerFactory,
    completionHandlerFactory: CompletionHandlerFactory[T]
  ) { factory =>

  def createProject(actorRefFactory: ActorRefFactory): ActorRef =
    actorRefFactory.actorOf(Props(
      new Project[T] {
        override val compilerFactory          = factory.compilerFactory
        override val sourceFileFactory        = factory.sourceFileFactory
        override val scalaSourcesFinder       = factory.scalaSourcesFinder
        override val configLoader             = factory.configLoader
        override val sourcesWatchActorFactory = factory.sourcesWatchActorFactory
        override val completionHandlerFactory = factory.completionHandlerFactory
      }
    ))
}
