package vim.scalacompletion

trait MemberRankCalculator[T] extends Function2[Option[String], T, Int]
