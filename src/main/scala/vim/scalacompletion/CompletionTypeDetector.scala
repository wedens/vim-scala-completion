package vim.scalacompletion

class CompletionTypeDetector {
  def detect(line: String, pos: Int): CompletionType = {
    // var1.<completion here>
    if (line.charAt(pos) == '.') {
      CompletionType.Type
    } else {
      val withoutSpacesReversed = line.reverse.dropWhile(_.isSpaceChar)
      withoutSpacesReversed.headOption match {
        // f(); <completion here>
        case Some(';') => CompletionType.Scope
        // <empty space><completion here>
        case None => CompletionType.Scope
        case _ => 
          val withoutLastWord = withoutSpacesReversed.dropWhile(!_.isSpaceChar)
          val preceedingSpace = withoutLastWord.nonEmpty

          if (preceedingSpace) {
            val withoutPreceedingSpaces = withoutLastWord.dropWhile(_.isSpaceChar)
            val charBeforeSpaces = withoutPreceedingSpaces.headOption
            charBeforeSpaces match {
              //<empty space>var1 <completion here>
              case None => CompletionType.Type 
              // var1;  cvar2  <completion here>
              case Some(';') => CompletionType.Type
              // infix method call
              // variable ! <completion here>
              case _ => CompletionType.Scope
            }
          // var1 <completion here>
          } else {
            CompletionType.Type
          }
      }
    }
  }
}
