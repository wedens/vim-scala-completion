package vim.scalacompletion.completion

case class MemberInfo(name: String, fullSignature: String,
  isConstructor: Boolean = false, isLocal: Boolean = false,
  isPublic: Boolean = false, isFromRootObjects: Boolean = true,
  isInherited: Boolean = false, isAccessible: Boolean = true)
