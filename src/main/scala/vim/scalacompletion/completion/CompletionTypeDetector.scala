package vim.scalacompletion.completion

import vim.scalacompletion.WithLog

import scala.reflect.api.Position

class CompletionTypeDetector extends WithLog {
  val scopeKeywords = Seq("if", "case", "new", "yield", "extends",
                          "with", "class", "trait", "val", "var", "def").map(_.reverse)



  def detect(position: Position): CompletionType = {
    detect(position.lineContent, position.column - 1)
  }

  def detect(line: String, pos: Int): CompletionType = {
    val charAtPosMsg = if (pos < line.length) line.charAt(pos) else "<none>"
    logg.debug(s"Detecting completion type at column: $pos ($charAtPosMsg), line: " +
      "\"" + line + "\"")

    val (beforePosAndPos, afterPos) = line.splitAt(pos + 1)
    val atPos = if (beforePosAndPos.nonEmpty) beforePosAndPos.last else ""
    if (pos >= line.length && atPos == '.') { // TODO: this is a dirty hack
      CompletionType.Type
    } else {
      val beforePos = if (beforePosAndPos.nonEmpty) beforePosAndPos.init else ""
      val lineBeforePosReversed = beforePos.reverse

      val notEscapedQuoteRegex = "\"(\\\\\"|[^\"])*".r
      val matches = notEscapedQuoteRegex.findAllMatchIn(beforePos).toSeq
      val balanced = matches.length % 2 == 0
      val insideOfString = !balanced
      if (insideOfString) {
        if (lineBeforePosReversed.startsWith("$")) {
          CompletionType.Scope
        } else {
          val openingQuotePosition = matches.last.start
          val interpolatedExprIdx = beforePos.indexOfSlice("${", openingQuotePosition)
          if (interpolatedExprIdx == -1) {
            CompletionType.NoCompletion
          } else {
            val exprStart = interpolatedExprIdx + 2
            val expr = beforePos.drop(exprStart)
            val posInExpr = pos - exprStart
            detect(expr, posInExpr)
          }
        }
      } else {
        detectInExpr(lineBeforePosReversed)
      }
    }
  }


  def detectInExpr(line: String) = {
    val trimmed = line.trim
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
      case Some(_) if emptyImport(line) => CompletionType.Scope
      // inside '{}' in import: import pkg.nest.{}
      case Some(_) if importSelectors(trimmed) => CompletionType.Type
      // complete infix method parameter
      case Some(_) if infixParameter(trimmed) => CompletionType.Scope
      // complete infix members
      case Some(ch) if ch.isLetterOrDigit => CompletionType.Type
      case _ => CompletionType.Scope
    }
  }

  def isIdentifierChar(ch: String) = {
    val positive = "[\\p{L}0-9\\p{Punct}\\p{Sm}]".r
    val exclude = "[^()\\[\\];.,{}\"'$]".r

    exclude.findFirstIn(ch).isDefined && positive.findFirstIn(ch).isDefined
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
