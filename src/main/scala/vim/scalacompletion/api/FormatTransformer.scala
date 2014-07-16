package vim.scalacompletion.api

trait FormatTransformer[From] {
  def transformCompletion(completions: Seq[From]): String
}
