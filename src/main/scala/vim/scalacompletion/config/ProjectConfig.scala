package vim.scalacompletion.config

import java.nio.file.Path

case class ProjectConfig(classpath: Set[Path], sourcesDirs: Set[Path])
