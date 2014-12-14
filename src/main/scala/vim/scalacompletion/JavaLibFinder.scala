package vim.scalacompletion

import java.nio.file.{Path, Paths}

trait JavaLibFinder {
  def locateJavaLib: Option[Path]
}
