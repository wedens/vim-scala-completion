package vim.scalacompletion

import java.io.{File => JFile}
import com.typesafe.config.{Config, ConfigFactory}

class ConfigLoader {
  def load(path: String): Config = ConfigFactory.parseFile(new JFile(path))
}
