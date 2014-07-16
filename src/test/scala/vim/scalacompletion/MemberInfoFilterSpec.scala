package vim.scalacompletion

import org.specs2.mutable._


class MemberInfoFilterSpec extends Specification {
  "member info filter" should {
    "filter out constructors" in {
      MemberInfoFilter(MemberInfo("", "", true)) must beFalse
    }
  }
}
