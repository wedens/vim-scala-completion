package vim.scalacompletion

trait Facade extends WithLog {
  type MemberInfoType

  val compilerApi: Compiler
  val completionTypeDetector: CompletionTypeDetector
  val extractor: compilerApi.Member => MemberInfoType
  val sourceFileFactory: SourceFileFactory

  def completeAt(name: String, path: String, offset: Int, column: Int): Seq[MemberInfoType] = {
    val source = sourceFileFactory.createSourceFile(name, path)
    compilerApi.addSources(List(source))
    val lineIdx = source.offsetToLine(offset)
    val sourceLine = source.lineToString(lineIdx)
    val position = source.position(offset)
    val completionType = completionTypeDetector.detect(sourceLine, column)
    logg.debug(s"Detected completion type $completionType for line $sourceLine at column: $column")
    logg.debug(s"Requesting completion $completionType at offset: $offset")
    completionType match {
      case CompletionType.Type => compilerApi.typeCompletion(position, extractor)
      case CompletionType.Scope => compilerApi.scopeCompletion(position, extractor)
      case _ => Seq.empty
    }
  }
}
