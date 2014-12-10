package vim.scalacompletion.api

import vim.scalacompletion.compiler.MemberInfo

class VimFormatTransformer extends FormatTransformer[MemberInfo] {
  def transformCompletion(completions: Seq[MemberInfo]) = {
    val listElements = completions.map { completion =>
      s"{'word': '${completion.name}', 'menu': '${completion.fullSignature}', " +
        s"'info': '${completion.fullSignature}'}"
    }

    s"[${listElements.mkString(", ")}]"
  }

  def transformImportSuggestions(className: String, packages: Set[String]) = {
    val fqcns = packages.map(_ + "." + className)
    s"[${fqcns.map(fqcn => "\"" + fqcn + "\"")mkString(", ")}]"
  }
}
