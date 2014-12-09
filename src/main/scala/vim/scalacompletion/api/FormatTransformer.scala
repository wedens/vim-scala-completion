package vim.scalacompletion.api

trait FormatTransformer[From] {
  def transformCompletion(completions: Seq[From]): String
  def transformImportSuggestions(className: String, packages: Set[String]): String
}
