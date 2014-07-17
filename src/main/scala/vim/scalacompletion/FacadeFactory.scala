package vim.scalacompletion

import java.io.{File => JFile}

trait FacadeFactory[T] {
  def createFacade(classpath: Seq[String]): Facade[T]
}

object FacadeFactoryImpl extends FacadeFactory[MemberInfo] {
  def createFacade(classpath: Seq[String]): Facade[MemberInfo] = {
    new Facade[MemberInfo] {
        val compilerApi = CompilerFactory(classpath.map(new JFile(_)))
        val extractor = MemberInfoExtractor(compilerApi)
        val completionTypeDetector = new CompletionTypeDetector
        val sourceFileFactory = new SourceFileFactoryImpl
        val membersFilter: MemberInfo => Boolean = MemberInfoFilter
        val memberRankCalculator: MemberRankCalculator[MemberInfo] = MemberRankCalculatorImpl
      }
  }
}
