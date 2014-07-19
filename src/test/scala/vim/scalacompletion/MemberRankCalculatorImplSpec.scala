package vim.scalacompletion

import org.specs2.mutable.Specification

class MemberRankCalculatorImplImplSpec extends Specification {
  val calculatorWithNonePrefix = (MemberRankCalculatorImpl.apply _).curried(None)

  "member rank calculator" should {
    "start with 0" in {
      calculatorWithNonePrefix(MemberInfo("a", "")) must_== 0
    }

    "add 10 if member is local" in {
      calculatorWithNonePrefix(MemberInfo("a", "", isLocal = true)) must_== 10
    }

    "add 5 if member is public" in {
      calculatorWithNonePrefix(MemberInfo("a", "", isPublic = true)) must_== 5
    }

    "add 10 if member is not inhereted from Any, Object etc" in {
      calculatorWithNonePrefix(MemberInfo("a", "", isFromRootObjects = false)) must_== 10
    }

    "add 10 if member name starts with prefix" in {
      MemberRankCalculatorImpl(Some("qwe"), MemberInfo("qwerty", "")) must_== 10
    }

    "not add 10 if member name starts with prefix" in {
      MemberRankCalculatorImpl(Some("xx"), MemberInfo("qwerty", "")) must_== 0
    }
  }
}
