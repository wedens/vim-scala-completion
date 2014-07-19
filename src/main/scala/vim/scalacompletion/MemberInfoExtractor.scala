package vim.scalacompletion

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.symtab.Flags

case class MemberInfo(name: String, fullSignature: String,
  isConstructor: Boolean = false, isLocal: Boolean = false,
  isPublic: Boolean = false, isFromRootObjects: Boolean = true)

object MemberInfoExtractor {
  def apply(compiler: Global): compiler.Member => MemberInfo = member => {
    import compiler.definitions

    val sym = member.sym
    val name = sym.nameString
    val fullSignature = sym.defStringSeenAs(member.tpe)
    val isConstructor = sym.isConstructor
    val isLocal = sym.isLocalToBlock
    val isPublic = sym.isPublic

    val isFromRootObjects = sym.owner == definitions.AnyClass ||
                           sym.owner == definitions.AnyRefClass ||
                           sym.owner == definitions.ObjectClass
    MemberInfo(name, fullSignature, isConstructor, isFromRootObjects)
  }
}
