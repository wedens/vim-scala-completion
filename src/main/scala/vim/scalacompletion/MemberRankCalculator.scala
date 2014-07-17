package vim.scalacompletion

trait MemberRankCalculator[T] extends Function2[Option[String], T, Int]

object MemberRankCalculatorImpl extends MemberRankCalculator[MemberInfo] {
  def apply(prefix: Option[String], member: MemberInfo): Int = {
    var rank = 0

    if (member.isLocal) {
      rank += 10
    }

    if (member.isPublic) {
      rank += 5
    }

    rank += prefix.filter(member.name.startsWith(_)).map(_ => 10) getOrElse 0

    rank
  }
}
