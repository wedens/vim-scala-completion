package vim.scalacompletion.completion

import vim.scalacompletion.WithLog

import scala.reflect.api.Position

class CompletionTypeDetector extends WithLog {
  val scopeKeywords = Seq("if", "case", "new", "yield", "extends",
                          "with", "class", "trait", "val", "var", "def").map(_.reverse)

  def isIdentifierChar(ch: String) = {
    val positive = "[\\p{L}0-9\\p{Punct}\\p{Sm}]".r
    val exclude = "[^()\\[\\];.,{}\"'$]".r

    exclude.findFirstIn(ch).isDefined && positive.findFirstIn(ch).isDefined
  }

  def detect(position: Position): CompletionType = {
    detect(position.lineContent, position.column - 1)
  }

  def detect(line: String, pos: Int): CompletionType = {
    val charAtPosMsg = if (pos < line.length) line.charAt(pos) else ""
    logg.debug(s"Detecting completion type at column: $pos ($charAtPosMsg), line: " +
      "\"" + line + "\"")

    val (beforePosAndPos, afterPos) = line.splitAt(pos + 1)
    val atPos = beforePosAndPos.last
    if (pos >= line.length && atPos == '.') {
      CompletionType.Type
    } else {
      val beforePos = beforePosAndPos.init
      val lineBeforePosReversed = beforePos.reverse

      val insideOfString = lineBeforePosReversed.count(_ == '"') % 2 != 0
      if (insideOfString) {
        lineBeforePosReversed.headOption match {
          case Some('$') => CompletionType.Scope
          case Some('{') if lineBeforePosReversed.charAt(1) == '$' => CompletionType.Scope
          case _ => CompletionType.NoCompletion
        }
      } else {
        val trimmed = lineBeforePosReversed.trim
        trimmed.headOption match {
          // type completion after identifier with following dot
          case Some('.') => CompletionType.Type
          // empty line before completion position
          case None => CompletionType.Scope
          // scope completion after ';' separator
          case Some(';') => CompletionType.Scope
          // after: 'if', 'with' etc
          case Some(_) if precedingKeyword(trimmed) => CompletionType.Scope
          // import without any selector: import
          case Some(_) if emptyImport(lineBeforePosReversed) => CompletionType.Scope
          // inside '{}' in import: import pkg.nest.{}
          case Some(_) if importSelectors(trimmed) => CompletionType.Type
          // complete infix method parameter
          case Some(_) if infixParameter(trimmed) => CompletionType.Scope
          // complete infix members
          case Some(ch) if ch.isLetterOrDigit => CompletionType.Type
          case _ => CompletionType.Scope
        }
      }
    }
  }


  private def infixParameter(str: String) = {
    val wordRemoved = str.dropWhile(!_.isSpaceChar)
    val somethingBeforeSpace = wordRemoved.length - 1 > 0
    if (somethingBeforeSpace) {
      val withoutSpace = wordRemoved.tail
      val looksLikeIdentifierBeforeSpace = withoutSpace.head.isLetterOrDigit
      looksLikeIdentifierBeforeSpace
    } else false
  }

  private def precedingKeyword(str: String) = scopeKeywords.exists(str.startsWith)
  private def importSelectors(str: String) = str.matches(".*\\{.* tropmi[\\s;]*")
  private def emptyImport(str: String) = str.matches("\\s+tropmi[\\s;]*")
}
