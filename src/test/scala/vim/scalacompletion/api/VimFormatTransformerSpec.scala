package vim.scalacompletion.api

import org.specs2.mutable.Specification
import vim.scalacompletion.compiler.MemberInfo

class VimFormatTransformerSpec extends Specification {
  val transformer = new VimFormatTransformer

  "vim transformer" should {
    "transform completion result to vim format" in {
      val completions = List(
        MemberInfo("var1", "Int"),
        MemberInfo("method", "def method[T]")
      )

      val result = transformer.transformCompletion(completions)

      result must_== "[{'word': 'var1', 'menu': 'Int', 'info': 'Int'}, " +
                     "{'word': 'method', 'menu': 'def method[T]', 'info': 'def method[T]'}]"
    }
  }
}
