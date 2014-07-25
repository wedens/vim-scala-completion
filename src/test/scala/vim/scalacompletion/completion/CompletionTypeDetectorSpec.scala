package vim.scalacompletion.completion

import org.specs2.mutable._
import org.specs2.specification.Scope
import org.specs2.mock._
import org.specs2.matcher.ThrownExpectations
import scala.reflect.api.Position
import scala.tools.nsc.interactive.Global
import vim.scalacompletion.compiler.Compiler
import vim.scalacompletion.compiler.CompilerApi
import CompletionType._
import CompletionContext._

trait detector extends Scope with ThrownExpectations with Mockito {
  val compiler: Global = mock[Compiler]
  val compilerApi: CompilerApi = mock[CompilerApi]

  val detector = new CompletionTypeDetector(compiler, compilerApi)

  val position = mock[Global#Position]
}

class CompletionTypeDetectorSpec extends Specification {
  "completion type detector" should {
    "detect type completion after import with selector" in new detector {
      val tree = compiler.Import(mock[compiler.Select], List())
      compilerApi.getTypeAt(position) returns tree

      detector.detectAt(position) must_== Type(Some(ImportContext))
    }

    "detect scope completion after import without selector" in new detector {
      val tree = compiler.Import(compiler.EmptyTree, List())
      compilerApi.getTypeAt(position) returns tree

      detector.detectAt(position) must_== Scope(Some(ImportContext))
    }

    "detect scope completion after 'with' or 'extends'" in new detector {
      val tree = compiler.Template(List(), compiler.noSelfType, List())
      compilerApi.getTypeAt(position) returns tree

      detector.detectAt(position) must_== Scope(Some(TypeNameContext))
    }

    "detect type completion after dot" in new detector {
      val tree = compiler.Select(compiler.EmptyTree, mock[compiler.Name])
      compilerApi.getTypeAt(position) returns tree

      detector.detectAt(position) must_== Type(Some(MemberSelectionContext))
    }

    "detect scope completion after 'new'" in new detector {
      val tree = compiler.New(compiler.EmptyTree)
      compilerApi.getTypeAt(position) returns tree

      detector.detectAt(position) must_== Scope(Some(NewContext))
    }

    "detect scope completion inside []" in new detector{
      val tree = compiler.Ident(mock[compiler.TypeName])
      compilerApi.getTypeAt(position) returns tree

      detector.detectAt(position) must_== Scope(Some(TypeNameContext))
    }

    "detect type after dot inside method application" in new detector {
      val tree = compiler.Apply(mock[compiler.Select], List())
      compilerApi.getTypeAt(position) returns tree

      detector.detectAt(position) must_== Type(Some(MemberSelectionContext))
    }

    "detect scope after 'new' inside method application" in new detector {
      val tree = compiler.Apply(mock[compiler.New], List())
      compilerApi.getTypeAt(position) returns tree

      detector.detectAt(position) must_== Scope(Some(NewContext))
    }

    // simple scope completion: Seq(1, 2).foldLeft[Int]($)
    // "detect scope in application parametrized method with type and normal parameters" in new detector {
    //   val tree = compiler.Apply(mock[compiler.TypeApply], List())
    //   compilerApi.getTypeAt(position) returns tree

    //   detector.detectAt(position) must_== Scope(Some(TypeNameContext))
    // }

    // Ident(TypeName) will work here
    // "detect scope in application parametrized method with only type parameters" in new detector {
    //   val tree = compiler.TypeApply(compiler.EmptyTree, List())
    //   compilerApi.getTypeAt(position) returns tree

    //   detector.detectAt(position) must_== Scope(Some(TypeNameContext))
    // }.pendingUntilFixed(". for some reason throws NPE at tree construction")
  }
}
