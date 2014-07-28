package vim.scalacompletion.completion

trait CompletionType
object CompletionTypes {
  case object Type extends CompletionType
  case object Scope extends CompletionType
  case object NoCompletion extends CompletionType
}
