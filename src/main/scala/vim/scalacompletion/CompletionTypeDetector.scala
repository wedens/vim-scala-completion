package vim.scalacompletion

import scala.reflect.api.Position

class CompletionTypeDetector {
  val scopeKeywords = Seq("case", "new", "yield", "extends", "with").map(_.reverse)

  def detect(position: Position): CompletionType = {
    detect(position.lineContent, position.column)
  }

  def detect(line: String, pos: Int): CompletionType = {
    val (beforePos, afterPos) = line.splitAt(pos + 1)
    val lineBeforePosReversed = beforePos.reverse

    def isInfix(str: String) = {
      val skipWord = str.dropWhile(!_.isSpaceChar)
      skipWord.headOption match {
        case Some(ch) if ch.isLetterOrDigit => true
        case _ => false
      }
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
            case Some(ch) if ch.isLetterOrDigit => CompletionType.Type
            case Some(_) if isInfix(withoutSpaces) => CompletionType.Scope
            case _ => CompletionType.Scope
          }
      }
    }
  }
}
