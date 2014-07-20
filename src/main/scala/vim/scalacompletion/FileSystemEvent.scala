package vim.scalacompletion

import java.io.File

object FileSystemEvents {
  trait FileSystemEvent {
    val fileOrDirectory: File
  }
  case class Created(fileOrDirectory: File) extends FileSystemEvent
  case class Deleted(fileOrDirectory: File) extends FileSystemEvent
  case class Modifyed(fileOrDirectory: File) extends FileSystemEvent
}
