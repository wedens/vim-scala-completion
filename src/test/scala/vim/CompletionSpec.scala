// package vim

// import org.specs2.mutable._
// import java.io.{File => JFile}
// import scala.tools.nsc.io._
// import scala.tools.nsc.interactive.Response
// import scala.tools.nsc.Settings
// import scala.tools.nsc.symtab._
// import scala.tools.nsc.reporters._
// import scala.reflect.internal.util.BatchSourceFile
// import scala.tools.nsc.reporters.{StoreReporter, ConsoleReporter}
// import scala.tools.nsc.interactive.Global

// class CompletionSpec extends Specification {
//   val rawCode = """object test extends App {
//     val x = "some string"
//     x.$
//   }
//   """
//   val code = removeLocator(rawCode)

//   def locatorPos(str: String) = str.indexOf("$")
//   def removeLocator(str: String) = str.patch(locatorPos(str), "", 1)

//   def createCompiler = {
//     val jars = Seq(new JFile("/usr/lib/jvm/java-7-oracle/jre/lib/rt.jar"), 
//       new JFile("/home/wedens/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.1.jar"),
//       new JFile("/home/wedens/.ivy2/cache/org.scalaz/scalaz-core_2.11/jars/scalaz-core_2.11-7.0.6.jar"))
//     val settings = new Settings()
//     val sep = JFile.pathSeparator
//     settings.classpath.value = jars.map(_.getAbsolutePath).mkString("", sep, "")

//     val reporter = new ConsoleReporter(settings)
//     new Global(settings, reporter)
//   }

//   def addSource(compiler: Global, sourceStr: String)  = {
//     val source = new BatchSourceFile("test", sourceStr)

//     val reloadResult = new Response[Unit]
//     compiler.askReload(List(source), reloadResult)
//     reloadResult.get
//     source
//   }

//   "compiler" should {
//     "suggest String members" in {
//       val compiler = createCompiler
//       val source = addSource(compiler, code)

//       val pos = source.position(locatorPos(rawCode))

//       val completeResult = new Response[List[compiler.Member]]
//       compiler.askTypeCompletion(pos, completeResult)
//       completeResult.get match {
//         case Left(matches) => 
//           compiler.ask { () =>
//             println(matches) 
//           }
//         case Right(ex) => throw ex
//       }
//       ok
//     }

//     "suggest scalaz cata for option" in {
//       val compiler = createCompiler
//       val rawCode = """import scalaz._; import syntax.std.option._;
//       object test extends App {
//         val opt = Option(1)
//         opt.$
//       }
//       """
//       val code = removeLocator(rawCode)
//       val source = addSource(compiler, code)
//       val pos = source.position(locatorPos(rawCode))

//       val completeResult = new Response[List[compiler.Member]]
//       compiler.askTypeCompletion(pos, completeResult)
//       completeResult.get match {
//         case Left(matches) => 
//           compiler.ask { () =>
//             println(matches) 
//           }
//         case Right(ex) => throw ex
//       }
//       ok
//     }
//   }
  
// }
