package vim.scalacompletion.completion

import vim.scalacompletion.compiler.MemberInfo

object MemberInfoFilter extends MemberFilter[MemberInfo] {
  def apply(prefix: Option[String], member: MemberInfo): Boolean = {
    val startsWithPrefix = prefix match {
      case Some(p) => member.name.startsWith(p)
      case None => true
    }
    !member.isConstructor && startsWithPrefix
  }
}
