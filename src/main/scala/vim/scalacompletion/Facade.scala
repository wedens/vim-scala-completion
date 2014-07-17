package vim.scalacompletion

trait Facade extends WithLog {
  type MemberInfoType

  val compilerApi: Compiler
  val completionTypeDetector: CompletionTypeDetector
  val extractor: compilerApi.Member => MemberInfoType
  val sourceFileFactory: SourceFileFactory
  val membersFilter: MemberInfoType => Boolean
  val memberRankCalculator: MemberRankCalculator[MemberInfoType]

  def completeAt(name: String, path: String, offset: Int,
          column: Int, prefix: Option[String]): Seq[MemberInfoType] = {
    val source = sourceFileFactory.createSourceFile(name, path)
    compilerApi.addSources(List(source))

    val lineIdx = source.offsetToLine(offset)
    val sourceLine = source.lineToString(lineIdx)
    val position = source.position(offset)

    val completionType = completionTypeDetector.detect(sourceLine, column)

    logg.debug(s"Detected completion type $completionType for line $sourceLine at column: $column (${if (sourceLine.length >= column) sourceLine.charAt(column) else "<none>"})")
    logg.debug(s"Requesting completion $completionType at offset: $offset, prefix: ${prefix.getOrElse("<none>")}")

    val completionResult = completionType match {
      case CompletionType.Type => compilerApi.typeCompletion(position, extractor)
      case CompletionType.Scope => compilerApi.scopeCompletion(position, extractor)
      case _ => Seq.empty
    }

    val filteredMembers = completionResult.view.filter(membersFilter)
    logg.debug(s"Found ${filteredMembers.length} members")

    val rankCalculatorWithPrefix = (memberRankCalculator.apply _).curried(prefix)
    val sortedByRank = filteredMembers
      .map(member => (member, rankCalculatorWithPrefix(member)))
      .sortBy { case (_, rank) => -rank }
      .take(15)
    sortedByRank.map { case (member, _) => member }.force
  }
}
