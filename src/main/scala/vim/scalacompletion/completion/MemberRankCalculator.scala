package vim.scalacompletion.completion

trait MemberRankCalculator[T] extends ((Option[String], T) => Int)
