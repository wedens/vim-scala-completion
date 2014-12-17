package vim.scalacompletion.completion

import scala.tools.nsc.interactive.Global

import scalaz._
import Scalaz._

trait MemberInfoModule {
  def memberInfoFrom(global: Global)(member: global.Member): MemberInfo = {
    import global.definitions

    val sym           = member.sym
    val name          = sym.nameString
    val fullSignature = member.forceInfoString
    val isConstructor = sym.isConstructor
    val isLocal       = sym.isLocalToBlock
    val isPublic      = sym.isPublic
    val isInherited   = member match {
      case tm: global.TypeMember => tm.inherited
      case _ => false
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

  def filterMember(prefix: Option[String])(member: MemberInfo): Boolean = {
    lazy val startsWithPrefix = prefix.map(member.name.startsWith)
    !member.isConstructor && member.isAccessible && startsWithPrefix.getOrElse(true)
  }

  def calculateMemberRank(prefix: Option[String])(member: MemberInfo): Int = {
    val ranks =
      (!member.isInherited).option(10) ::
      member.isLocal.option(20) ::
      member.isPublic.option(10) ::
      (!member.isFromRootObjects).option(30) ::
      prefix.map(member.name.length - _.length) :: Nil

    ranks.flatten.sum
  }

  def orderByRankDesc(prefix: Option[String]): scala.math.Ordering[MemberInfo]  =
    scala.math.Ordering.by(calculateMemberRank(prefix)(_))
}
