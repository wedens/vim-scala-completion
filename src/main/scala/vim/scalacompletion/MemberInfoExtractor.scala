package vim.scalacompletion

import scala.tools.nsc.interactive.Global
import scala.tools.nsc.symtab.Flags

case class MemberInfo(name: String, fullSignature: String,
  isConstructor: Boolean = false, isLocal: Boolean = false,
  isPublic: Boolean = false, isFromRootObjects: Boolean = true,
  isInherited: Option[Boolean] = None)

trait MemberInfoExtractorFactory[T] {
  def create(compiler: Global): MemberInfoExtractor[T]
}

class MemberInfoExtractorFactoryImpl extends MemberInfoExtractorFactory[MemberInfo] {
  def create(compiler: Global): MemberInfoExtractor[MemberInfo] = new MemberInfoExtractorForMemberInfo(compiler)
}

trait MemberInfoExtractor[T] extends (Global#Member => T)

class MemberInfoExtractorForMemberInfo(compiler: Global) extends MemberInfoExtractor[MemberInfo] {
  def apply(member: Global#Member): MemberInfo = {
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

    MemberInfo(name, fullSignature,
      isConstructor, isLocal,
      isPublic, isFromRootObjects,
      isInherited)
  }
}

