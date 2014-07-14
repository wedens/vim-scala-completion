package vim.scalacompletion

trait CompletionType
object CompletionType {
  case object Type extends CompletionType
  case object Scope extends CompletionType
  case object NoCompletion extends CompletionType
}
