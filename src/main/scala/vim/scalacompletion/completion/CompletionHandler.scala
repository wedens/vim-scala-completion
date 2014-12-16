// package vim.scalacompletion.completion

// import vim.scalacompletion._
// import vim.scalacompletion.compiler.{MemberInfoExtractor, MemberInfo, Compiler}

// import scala.reflect.internal.util.Position

// class CompletionHandlerFactory[T](
//     completionTypeDetector: CompletionTypeDetector,
//     extractor: MemberInfoExtractor[T],
//     membersFilter: MemberFilter[T],
//     memberRankCalculator: MemberRankCalculator[T]
//   ) {

//   def create(compiler: Compiler): CompletionHandler[T] =
//     new CompletionHandler[T](completionTypeDetector, compiler,
//                   extractor, membersFilter, memberRankCalculator)
// }

// class CompletionHandler[T](
//                 completionTypeDetector: CompletionTypeDetector,
//                 compiler: Compiler,
//                 extractor: MemberInfoExtractor[T],
//                 membersFilter: MemberFilter[T],
//                 memberRankCalculator: MemberRankCalculator[T]) extends WithLog {
//   val extractorBound = extractor(compiler)

//   def complete(positionAfter: Position, prefix: Option[String] = None,
//                                    maxResults: Option[Int] = None): Seq[T] = {
//     val completionType = completionTypeDetector.detect(positionAfter)
//     val members = completionType match {
//       case CompletionTypes.Type =>
//         val pointAtCompletionPos = positionAfter.point - 1
//         val completionPos = positionAfter.withPoint(pointAtCompletionPos)
//         compiler.typeCompletion(completionPos, extractorBound)
//       case CompletionTypes.Scope => compiler.scopeCompletion(positionAfter, extractorBound)
//       case _ => Seq.empty
//     }

//     val membersFilterWithPrefix = (membersFilter.apply _).curried(prefix)
//     val filteredMembers = members.filter(membersFilterWithPrefix)
//     logg.debug(s"$completionType: Found ${members.length} members. ${filteredMembers.length} filtered.")

//     val rankCalculatorWithPrefix = (memberRankCalculator.apply _).curried(prefix)
//     val sortedByRank = filteredMembers.sortBy(-rankCalculatorWithPrefix(_))

//     maxResults.map(sortedByRank.take) getOrElse sortedByRank
//   }
// }
