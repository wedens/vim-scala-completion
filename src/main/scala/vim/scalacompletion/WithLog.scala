package vim.scalacompletion

import org.slf4j.LoggerFactory

trait WithLog {
  lazy val logg = LoggerFactory.getLogger(this.getClass)
}
