package vim.scalacompletion.completion

trait MemberFilter[T] extends ((Option[String], T) => Boolean)
