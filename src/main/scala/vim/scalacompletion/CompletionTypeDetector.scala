package vim.scalacompletion

class CompletionTypeDetector {
  def detect(line: String, pos: Int): CompletionType = {
    val (beforePos, afterPos) = line.splitAt(pos + 1)
    val lineBeforePosReversed = beforePos.reverse

    lineBeforePosReversed.headOption match {
      case Some('.') => CompletionType.Type
      case None => CompletionType.Scope
    }
    // // var1.<completion here>
    // if (line.charAt(pos) == '.') {
    //   CompletionType.Type
    // } else {
    //   val withoutCodeAfterPos = line.take(pos + 1)
    //   val withoutSpacesReversed = withoutCodeAfterPos.reverse.dropWhile(_.isSpaceChar)
    //   withoutSpacesReversed.headOption match {
    //     // f(); <completion here>
    //     case Some(';') => CompletionType.Scope
    //     // <empty space><completion here>
    //     case None => CompletionType.Scope
    //     case _ => 
    //       val withoutLastWord = withoutSpacesReversed.dropWhile(!_.isSpaceChar)
    //       val preceedingSpace = withoutLastWord.nonEmpty

    //       if (preceedingSpace) {
    //         val withoutPreceedingSpaces = withoutLastWord.dropWhile(_.isSpaceChar)
    //         val charBeforeSpaces = withoutPreceedingSpaces.headOption
    //         charBeforeSpaces match {
    //           //<empty space>var1 <completion here>
    //           case None => CompletionType.Type 
    //           // var1;  cvar2  <completion here>
    //           case Some(';') => CompletionType.Type
    //           // infix method call
    //           // variable ! <completion here>
    //           case Some(ch) if ch.isLetterOrDigit => CompletionType.Scope
    //           case _ => CompletionType.Type
    //         }
    //       // var1 <completion here>
    //       } else {
    //         CompletionType.Type
    //       }
    //   }
    // }
  }
}
