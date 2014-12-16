package vim.scalacompletion.config

import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}

import scalaz._
import Scalaz._

trait ConfigurationModule {
  lazy val configReader = new ConfigurationReader
  class ConfigurationReader {
    private def loadConfig(configPath: Path): Throwable \/ Config =
      \/.fromTryCatchNonFatal {
        ConfigFactory.parseFile(configPath.toFile)
      }

    private def readConfig(config: Config): Throwable \/ ProjectConfig =
      for {
        classpath <- \/.fromTryCatchNonFatal(
          config.getStringList("vim.scala-completion.classpath"))
        sourcesDirs <- \/.fromTryCatchNonFatal(
          config.getStringList("vim.scala-completion.src-directories"))
      } yield {
        ProjectConfig(
          classpath.map(Paths.get(_)).toSet,
          sourcesDirs.map(Paths.get(_)).toSet
        )
      }

    def parseConfig(configPath: Path): Throwable \/ ProjectConfig =
      loadConfig(configPath) >>= readConfig
  }
}
