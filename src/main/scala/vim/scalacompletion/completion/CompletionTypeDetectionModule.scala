package vim.scalacompletion.completion

trait CompletionTypeDetectionModule {
  sealed trait CompletionType
  case object Scope extends CompletionType
  case object Type extends CompletionType
  case object NoCompletion extends CompletionType

  lazy val completionTypeDetector = new CompletionTypeDetector
  class CompletionTypeDetector {
    private val scopeKeywords = Seq("if", "else", "case", "new", "yield", "extends",
      "with", "class", "trait", "val", "var", "def").map(_.reverse)

    def detectAt(line: String, pos: Int): CompletionType = {
      val (beforePosAndPos, afterPos) = line.splitAt(pos + 1)
      val atPos = if (beforePosAndPos.nonEmpty) beforePosAndPos.last else ""
      if (pos >= line.length && atPos == '.') { // TODO: this is a dirty hack
        Type
      } else {
        val beforePos = if (beforePosAndPos.nonEmpty) beforePosAndPos.init else ""
        val lineBeforePosReversed = beforePos.reverse

        val notEscapedQuoteRegex = "\"(\\\\\"|[^\"])*".r
        val matches = notEscapedQuoteRegex.findAllMatchIn(beforePos).toSeq
        val balanced = matches.length % 2 == 0
        val insideOfString = !balanced
        if (insideOfString) {
          if (lineBeforePosReversed.startsWith("$")) {
            Scope
          } else {
            val openingQuotePosition = matches.last.start
            val interpolatedExprIdx = beforePos.indexOfSlice("${", openingQuotePosition)
            if (interpolatedExprIdx == -1) {
              NoCompletion
            } else {
              val exprStart = interpolatedExprIdx + 2
              val expr = beforePos.drop(exprStart)
              val posInExpr = pos - exprStart
              detectAt(expr, posInExpr)
            }
          }
        } else {
          detectInExpr(lineBeforePosReversed)
        }
      }
    }

    private def detectInExpr(line: String) = {
      val trimmed = line.trim
      trimmed.headOption match {
        // type completion after identifier with following dot
        case Some('.') => Type
        // empty line before completion position
        case None => Scope
        // scope completion after ';' separator
        case Some(';') => Scope
        // after: 'if', 'with' etc
        case Some(_) if precedingKeyword(trimmed) => Scope
        // import without any selector: import
        case Some(_) if emptyImport(line) => Scope
        // inside '{}' in import: import pkg.nest.{}
        case Some(_) if importSelectors(trimmed) => Type
        // complete infix method parameter
        case Some(_) if infixParameter(trimmed) => Scope
        // complete infix members
        case Some(ch) if ch.isLetterOrDigit => Type
        case _ => Scope
      }
    }

    private def isIdentifierChar(ch: String) = {
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
}
