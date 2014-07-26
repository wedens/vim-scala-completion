package vim.scalacompletion.completion

import vim.scalacompletion._
import vim.scalacompletion.compiler.{MemberInfoExtractorFactory, MemberInfoExtractor, MemberInfo, Compiler}

import scala.reflect.internal.util.Position

trait CompletionHandlerFactory[T] {
  def create(compiler: Compiler): CompletionHandler[T]
}

class CompletionHandlerFactoryForMemberInfo(
      memberInfoExtractorFactory: MemberInfoExtractorFactory[MemberInfo]
    ) extends CompletionHandlerFactory[MemberInfo] {

  def create(compiler: Compiler): CompletionHandler[MemberInfo] =
    new CompletionHandler(new CompletionTypeDetector(compiler, compiler), compiler,
      memberInfoExtractorFactory.create(compiler), MemberInfoFilter,
      MemberRankCalculatorImpl, new PositionFactory)
}

class CompletionHandler[T](
                completionTypeDetector: CompletionTypeDetector,
                compiler: Compiler,
                extractor: MemberInfoExtractor[T],
                membersFilter: MemberFilter[T],
                memberRankCalculator: MemberRankCalculator[T],
                positionFactory: PositionFactory) extends WithLog {

  import scala.reflect.internal.util.BatchSourceFile
  import scala.reflect.internal.util.SourceFile

  private def editSource(positionAfter: Position): Option[Position] = {
    val source = positionAfter.source
    val content = source.content
    val pointAtCompletion = positionAfter.point - 1
    val pointAfterCompletion = positionAfter.point
    val charAtCompletionPosition = content(pointAtCompletion)
    if (charAtCompletionPosition == ' ') { // TODO: better detection
      logg.info(s"Infix member selection. Replacing space with dot")
      content(pointAtCompletion) = '.'
    }

    val isMemberSelection = charAtCompletionPosition == '.' || charAtCompletionPosition == ' '
    if (isMemberSelection) {
      logg.info(s"Replacing member selection with ().")
      val (l, r) = content.splitAt(pointAfterCompletion)
      val updatedContent = l ++ Array('(', ')') ++ r
      val updatedSource = new BatchSourceFile(source.file, updatedContent)
      val updatedPositionAfter = positionAfter.withSource(updatedSource)
      Some(updatedPositionAfter)
    } else None
  }

  def complete(position: Position, prefix: Option[String] = None,
                                   maxResults: Option[Int] = None): Seq[T] = {

    val edited = editSource(position)
    edited.foreach { editedPos =>
      compiler.reloadSources(List(editedPos.source))
    }

    val typeDetectionPos = edited getOrElse position
    val completionPos  = typeDetectionPos.withPoint(typeDetectionPos.point - 1)

    logg.info(s"Detecting completion type at column: ${typeDetectionPos.column} (${typeDetectionPos.lineContent.charAt(typeDetectionPos.column - 1)})")
    val completionType = completionTypeDetector.detectAt(typeDetectionPos)
    logg.info(s"Completion $completionType at column: ${completionPos.column} (${completionPos.lineContent.charAt(completionPos.column - 1)}). Line: ${completionPos.lineContent}")
    val members = completionType match {
      case CompletionTypes.Type(_)  => compiler.typeCompletion(completionPos, extractor)
      case CompletionTypes.Scope(_) => compiler.scopeCompletion(completionPos, extractor)
      case _ => Seq.empty
    }

    val membersFilterWithPrefix = (membersFilter.apply _).curried(prefix)
    val filteredMembers = members.filter(membersFilterWithPrefix)
    logg.debug(s"$completionType: Found ${members.length} members. ${filteredMembers.length} filtered.")

    val rankCalculatorWithPrefix = (memberRankCalculator.apply _).curried(prefix)
    val sortedByRank = filteredMembers.sortBy(-rankCalculatorWithPrefix(_))

    maxResults.map(sortedByRank.take) getOrElse sortedByRank
  }
}
