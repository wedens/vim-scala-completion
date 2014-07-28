package vim.scalacompletion.filesystem

import java.io.File

trait FileSystemEvent {
  val fileOrDirectory: File
}

object FileSystemEvents {
  case class Created(override val fileOrDirectory: File) extends FileSystemEvent
  case class Deleted(override val fileOrDirectory: File) extends FileSystemEvent
  case class Modified(override val fileOrDirectory: File) extends FileSystemEvent
}
