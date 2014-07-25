package vim.scalacompletion.completion

trait CompletionType {
  val context: Option[CompletionContext]
}

object CompletionType {
  case class Scope(context: Option[CompletionContext] = None) extends CompletionType
  case class Type(context: Option[CompletionContext] = None) extends CompletionType
  case object NoCompletion extends CompletionType {
    override val context: Option[CompletionContext] = None
  }
}

trait CompletionContext

object CompletionContext {
  case object ImportContext extends CompletionContext
  case object NewContext extends CompletionContext
  case object TypeNameContext extends CompletionContext
  case object MemberSelectionContext extends CompletionContext
  case object ApplicationContext extends CompletionContext
}

