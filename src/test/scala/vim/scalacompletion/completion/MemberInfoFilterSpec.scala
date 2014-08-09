package vim.scalacompletion.completion

import org.specs2.mutable._
import vim.scalacompletion.compiler.MemberInfo


class MemberInfoFilterSpec extends Specification {
  "member info filter" should {
    "filter out constructors" in {
      MemberInfoFilter(None, MemberInfo("", "", true)) must beFalse
    }

    "not filter out not constructors" in {
      MemberInfoFilter(None, MemberInfo("", "", false)) must beTrue
    }

    "filter out member that not starts with prefix" in {
      MemberInfoFilter(Some("pfx"), MemberInfo("abcd", "", false)) must beFalse
    }

    "not filter out member that starts with prefix" in {
      MemberInfoFilter(Some("pfx"), MemberInfo("pfxxxx", "", false)) must beTrue
    }

    "filter out inaccessible members" in {
      MemberInfoFilter(None, MemberInfo("", "", isAccessible = false)) must beFalse
    }
  }
}
