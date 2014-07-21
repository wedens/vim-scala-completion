package vim.scalacompletion

object MemberRankCalculatorImpl extends MemberRankCalculator[MemberInfo] {
  def apply(prefix: Option[String], member: MemberInfo): Int = {
    var rank = 0

    if (member.isLocal) rank += 10
    if (member.isPublic) rank += 5
    if (!member.isFromRootObjects) rank += 10
    rank += member.isInherited.filterNot(isInherited => isInherited).map(_ => 10) getOrElse 0
    rank += prefix.filter(member.name.startsWith(_)).map(_ => 10) getOrElse 0

    rank
  }
}
