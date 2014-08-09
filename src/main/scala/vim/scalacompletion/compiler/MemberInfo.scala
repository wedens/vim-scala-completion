package vim.scalacompletion.compiler

case class MemberInfo(name: String, fullSignature: String,
  isConstructor: Boolean = false, isLocal: Boolean = false,
  isPublic: Boolean = false, isFromRootObjects: Boolean = true,
  isInherited: Option[Boolean] = None, isAccessible: Boolean = true)
