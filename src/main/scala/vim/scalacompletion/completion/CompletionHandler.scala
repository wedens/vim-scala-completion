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
    new CompletionHandler(new CompletionTypeDetector, compiler,
      memberInfoExtractorFactory.create(compiler), MemberInfoFilter,
      MemberRankCalculatorImpl)
}

class CompletionHandler[T](
                completionTypeDetector: CompletionTypeDetector,
                compiler: Compiler,
                extractor: MemberInfoExtractor[T],
                membersFilter: MemberFilter[T],
                memberRankCalculator: MemberRankCalculator[T]) extends WithLog {

  def complete(positionAfter: Position, prefix: Option[String] = None,
                                   maxResults: Option[Int] = None): Seq[T] = {
    val completionType = completionTypeDetector.detect(positionAfter)
    val members = completionType match {
      case CompletionType.Type =>
        val pointAtCompletionPos = positionAfter.point - 1
        val completionPos = positionAfter.withPoint(pointAtCompletionPos)
        compiler.typeCompletion(completionPos, extractor)
      case CompletionType.Scope => compiler.scopeCompletion(positionAfter, extractor)
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
