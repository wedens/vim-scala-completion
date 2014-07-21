package vim.scalacompletion

import org.specs2.mutable.Specification

class MemberRankCalculatorImplSpec extends Specification {
  val calculatorWithNonePrefix = (MemberRankCalculatorImpl.apply _).curried(None)

  val base = 100

  "member rank calculator" should {
    "start with 100" in {
      calculatorWithNonePrefix(MemberInfo("a", "")) must_== base
    }

    "add 10 if member is local" in {
      calculatorWithNonePrefix(MemberInfo("a", "", isLocal = true)) must_== base + 10
    }

    "add 5 if member is public" in {
      calculatorWithNonePrefix(MemberInfo("a", "", isPublic = true)) must_== base + 5
    }

    "add 10 if member is not inhereted from Any, Object etc" in {
      calculatorWithNonePrefix(MemberInfo("a", "", isFromRootObjects = false)) must_== base + 10
    }

    "add 10 if member is not inhereted from Any, Object etc" in {
      calculatorWithNonePrefix(MemberInfo("a", "", isInherited = Some(false))) must_== base + 10
    }

    // Don't need as we filter out members that not starts with prefix
    // "add 10 if member name starts with prefix" in {
    //   MemberRankCalculatorImpl(Some("qwerty"), MemberInfo("qwerty", "")) must_== base + 10
    // }

    // "not add 10 if member name starts with prefix" in {
    //   MemberRankCalculatorImpl(Some("xx"), MemberInfo("qwerty", "")) must_== base
    // }

    "substract difference between prefix length and member name length" in {
      MemberRankCalculatorImpl(Some("qwe"), MemberInfo("qwerty", "")) must_== base - 3
    }
  }
}
