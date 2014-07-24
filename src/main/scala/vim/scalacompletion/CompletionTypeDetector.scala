package vim.scalacompletion

import scala.reflect.api.Position

class CompletionTypeDetector extends WithLog {
  val scopeKeywords = Seq("case", "new", "yield", "extends", "with").map(_.reverse)

  def detect(position: Position): CompletionType = {
    detect(position.lineContent, position.column - 1)
  }

  def detect(line: String, pos: Int): CompletionType = {
    val charAtPosMsg = if (pos < line.length) line.charAt(pos) else ""
    logg.debug(s"Detecting completion type at column: $pos ($charAtPosMsg), line: " +
      "\"" + line + "\"")

    val (beforePosAndPos, afterPos) = line.splitAt(pos + 1)
    // val atPos = beforePosAndPos.last
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
      lineBeforePosReversed.headOption match {
        case Some('.') => CompletionType.Type
        case _ =>
          val withoutSpaces = lineBeforePosReversed.dropWhile(_.isSpaceChar)
          withoutSpaces.headOption match {
            case None => CompletionType.Scope
            case Some(';') => CompletionType.Scope
            case Some(_) if scopeKeywords.exists(withoutSpaces.startsWith(_)) => CompletionType.Scope
            // .*case .* if
            case Some(_) if withoutSpaces.matches("fi .* esac.*") => CompletionType.Scope
            // .*import .*{.*
            case Some(_) if withoutSpaces.matches(".*\\{.* tropmi.*") => CompletionType.Type
            case Some(_) if isInfix(withoutSpaces) => CompletionType.Scope
            case Some(ch) if ch.isLetterOrDigit => CompletionType.Type
            case _ => CompletionType.Scope
          }
      }
    }
  }
}
