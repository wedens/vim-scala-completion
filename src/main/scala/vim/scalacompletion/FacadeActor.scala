package vim.scalacompletion

import java.io.{File => JFile}
import akka.actor.Actor

object FacadeActor {
  case class CompleteAt(name: String, path: String,
    offset: Int, column: Int, prefix: Option[String])
  case class CompletionResult[T](members: Seq[T])

  case class ReloadSourcesInDirs(dirs: Seq[String])
  case class ReloadSources(sources: Seq[JFile])
  case class RemoveSources(sources: Seq[JFile])
}

trait FacadeActor[MemberInfoType] extends Actor with WithLog {
  import FacadeActor._

  val compilerApi: Compiler
  val completionTypeDetector: CompletionTypeDetector
  val extractor: compilerApi.Member => MemberInfoType
  val sourceFileFactory: SourceFileFactory
  val membersFilter: MemberFilter[MemberInfoType]
  val memberRankCalculator: MemberRankCalculator[MemberInfoType]
  val scalaSourcesFinder: ScalaSourcesFinder

  def receive = {
    case CompleteAt(name, path, offset, column, prefix) =>
      sender ! CompletionResult(completeAt(name, path, offset, column, prefix))
    case ReloadSourcesInDirs(dirs) =>
      reloadAllSourcesInDirs(dirs)
  }

  def completeAt(name: String, path: String, offset: Int,
          column: Int, prefix: Option[String]): Seq[MemberInfoType] = {
    val source = sourceFileFactory.createSourceFile(name, path)
    compilerApi.addSources(List(source))

    val lineIdx = source.offsetToLine(offset)
    val sourceLine = source.lineToString(lineIdx)
    val completionType = completionTypeDetector.detect(sourceLine, column)

    val charAtCompletionPos = if (sourceLine.length >= column) "(char: " + sourceLine.charAt(column) + ")" else ""
    logg.debug(s"Requested completion $completionType at offset: $offset, column: $column $charAtCompletionPos ${prefix.map(p => s" with prefix: $p)") getOrElse ""}")

    val completionResult = completionType match {
      case CompletionType.Type =>
        val offsetBeforeDotOrSpace = offset - 1
        val position = source.position(offsetBeforeDotOrSpace)
        compilerApi.typeCompletion(position, extractor)
      case CompletionType.Scope =>
        val position = source.position(offset)
        compilerApi.scopeCompletion(position, extractor)
      case _ => Seq.empty
    }

    val membersFilterWithPrefix = (membersFilter.apply _).curried(prefix)
    val filteredMembers = completionResult.view.filter(membersFilterWithPrefix)
    logg.debug(s"Found ${completionResult.length} members. ${filteredMembers.length} filtered.")

    val rankCalculatorWithPrefix = (memberRankCalculator.apply _).curried(prefix)
    val sortedByRank = filteredMembers
      .map(member => (member, rankCalculatorWithPrefix(member)))
      .sortBy { case (_, rank) => -rank }
      .take(15)
    sortedByRank.map { case (member, _) => member }.force
  }

  def reloadAllSourcesInDirs(dirs: Seq[String]) = {
    val sourcesJFiles = scalaSourcesFinder.findIn(dirs.map(new JFile(_)).toList)
    val sources = sourcesJFiles.map { file =>
      val canonicalPath = file.getCanonicalPath
      sourceFileFactory.createSourceFile(canonicalPath)
    }.toList
    compilerApi.addSources(sources)
  }
}
