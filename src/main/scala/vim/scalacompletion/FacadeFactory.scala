package vim.scalacompletion

import java.io.{File => JFile}

trait FacadeFactory[T] {
  def createFacade(classpath: Seq[String]): FacadeActor[T]
}

object FacadeFactoryImpl extends FacadeFactory[MemberInfo] {
  def createFacade(classpath: Seq[String]): FacadeActor[MemberInfo] = {
    new FacadeActor[MemberInfo] {
      val compilerApi = CompilerFactory(classpath.map(new JFile(_)))
      val extractor = MemberInfoExtractor(compilerApi)
      val completionTypeDetector = new CompletionTypeDetector
      val sourceFileFactory = new SourceFileFactoryImpl
      val membersFilter: MemberFilter[MemberInfo] = MemberInfoFilter
      val memberRankCalculator: MemberRankCalculator[MemberInfo] = MemberRankCalculatorImpl
      val scalaSourcesFinder: ScalaSourcesFinder = new ScalaSourcesFinder
    }
  }
}
