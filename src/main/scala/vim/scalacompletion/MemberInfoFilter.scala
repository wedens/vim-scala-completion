package vim.scalacompletion

object MemberInfoFilter extends (MemberInfo => Boolean) {
  def apply(member: MemberInfo): Boolean = {
    !member.isConstructor
  }
}
