package vim.scalacompletion.config

import java.nio.file.{Path, Paths}
import com.typesafe.config.{Config, ConfigFactory}
import collection.JavaConversions._

trait ConfigurationModule {
  case class ProjectConfig(classpath: Set[Path], sourcesDirs: Set[Path])

  def configLoader: ConfigLoader

  trait ConfigLoader {
    def parseConfig(configPath: Path): ProjectConfig
  }
}

trait TypesafeConfigurationModule extends ConfigurationModule {
  lazy val configLoader = new TypesafeConfigLoader

  class TypesafeConfigLoader extends ConfigLoader {

    private lazy val loadConfig: Path => Config =
      configPath => ConfigFactory.parseFile(configPath.toFile)

    private lazy val readConfig: Config => ProjectConfig = config => {
      val classpath = config.getStringList("vim.scala-completion.classpath")
      val sourcesDirs = config.getStringList("vim.scala-completion.src-directories")
      ProjectConfig(
        classpath.map(Paths.get(_)).toSet,
        sourcesDirs.map(Paths.get(_)).toSet
      )
    }

    override def parseConfig(configPath: Path): ProjectConfig =
      readConfig(loadConfig(configPath))
  }
}
