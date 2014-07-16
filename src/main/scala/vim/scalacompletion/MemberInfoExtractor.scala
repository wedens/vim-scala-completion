package vim.scalacompletion

import scala.tools.nsc.interactive.Global

case class MemberInfo(name: String, fullSignature: String)

object MemberInfoExtractor {
  def apply(compiler: Global): compiler.Member => MemberInfo = member => {
    val name = member.sym.nameString
    val fullSignature = member.sym.defStringSeenAs(member.tpe)
    MemberInfo(name, fullSignature)
  }
}