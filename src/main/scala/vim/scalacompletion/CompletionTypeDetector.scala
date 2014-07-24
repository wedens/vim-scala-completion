package vim.scalacompletion

import scala.reflect.api.Position

class CompletionTypeDetector extends WithLog {
  val scopeKeywords = Seq("if", "case", "new", "yield", "extends", "with").map(_.reverse)

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
    if (pos >= line.length && atPos == '.') return CompletionType.Type
    val beforePos = beforePosAndPos.init
    val lineBeforePosReversed = beforePos.reverse

    def isInfix(str: String) = {
      val wordRemoved = str.dropWhile(!_.isSpaceChar)
      val somethingBeforeSpace = wordRemoved.length - 1 > 0
      if (somethingBeforeSpace) {
        val withoutSpace = wordRemoved.tail
        val looksLikeIdentifierBeforeSpace = withoutSpace.head.isLetterOrDigit
        looksLikeIdentifierBeforeSpace
      } else false
    }

    val insideOfString = lineBeforePosReversed.count(_ == '"') % 2 != 0
    if (insideOfString) {
      lineBeforePosReversed.headOption match {
        case Some('$') => CompletionType.Scope
        case _ => CompletionType.NoCompletion
      }
    } else {
      val withoutSpaces = lineBeforePosReversed.dropWhile(_.isSpaceChar)
      withoutSpaces.headOption match {
        // type completion after identifier with following dot
        case Some('.') => CompletionType.Type
        // empty line before completion position
        case None => CompletionType.Scope
        // scope completion after ';' separator
        case Some(';') => CompletionType.Scope
        // after: 'if', 'with' etc
        case Some(_) if scopeKeywords.exists(withoutSpaces.startsWith(_)) => CompletionType.Scope
        // inside '{}' in import: import pkg.nest.{}
        // TODO: better regex
        case Some(_) if withoutSpaces.matches(".*\\{.* tropmi.*") => CompletionType.Type
        // complete infix method parameter
        case Some(_) if isInfix(withoutSpaces) => CompletionType.Scope
        // complete infix members
        case Some(ch) if ch.isLetterOrDigit => CompletionType.Type
        case _ => CompletionType.Scope
      }
    }
  }
}
