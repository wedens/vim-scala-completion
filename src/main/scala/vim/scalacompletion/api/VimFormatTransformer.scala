package vim.scalacompletion.api

import vim.scalacompletion.MemberInfo

class VimFormatTransformer extends FormatTransformer[MemberInfo] {
  def transformCompletion(completions: Seq[MemberInfo]) = {
    val listElements = completions.map { completion =>
      s"{'word': '${completion.name}', 'menu': '${completion.fullSignature}', " +
        s"'info': '${completion.fullSignature}'}"
    }

    s"[${listElements.mkString(", ")}]"
  }
}
