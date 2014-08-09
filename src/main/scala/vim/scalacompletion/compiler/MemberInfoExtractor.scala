package vim.scalacompletion.compiler

import scala.tools.nsc.interactive.Global

trait MemberInfoExtractor[T] extends (Global => Global#Member => T)

class MemberInfoExtractorForMemberInfo extends MemberInfoExtractor[MemberInfo] {
  def apply(compiler: Global) = (member: Global#Member) => {
    import compiler.definitions

    val sym = member.sym
    val name = sym.nameString
    val fullSignature = member.forceInfoString
    val isConstructor = sym.isConstructor
    val isLocal = sym.isLocalToBlock
    val isPublic = sym.isPublic
    val isInherited = member match {
      case tm: Compiler#TypeMember => Some(tm.inherited)
      case _ => None
    }

    val isFromRootObjects = sym.owner == definitions.AnyClass ||
                           sym.owner == definitions.AnyRefClass ||
                           sym.owner == definitions.ObjectClass

    val isAccessible = member.accessible

    MemberInfo(name, fullSignature,
      isConstructor, isLocal,
      isPublic, isFromRootObjects,
      isInherited, isAccessible)
  }
}

