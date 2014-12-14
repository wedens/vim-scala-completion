package vim.scalacompletion

import java.nio.file.{Path, Paths}

trait JavaLibFinderFromBootClasspath extends JavaLibFinder {
  lazy val locateJavaLib: Option[Path] = {
    System.getProperty("sun.boot.class.path")
      .split(":")
      .find(_.endsWith("rt.jar"))
      .map(Paths.get(_))
  }
}
