package vim.scalacompletion

trait MemberFilter[T] extends Function2[Option[String], T, Boolean]
