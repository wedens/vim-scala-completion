package vim.scalacompletion

import java.io.File

object FileSystemEvents {
  trait FileSystemEvent {
    val fileOrDirectory: File
  }
  case class Created(override val fileOrDirectory: File) extends FileSystemEvent
  case class Deleted(override val fileOrDirectory: File) extends FileSystemEvent
  case class Modifyed(override val fileOrDirectory: File) extends FileSystemEvent
}
