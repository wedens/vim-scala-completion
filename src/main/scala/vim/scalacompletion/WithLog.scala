package vim.scalacompletion

import org.slf4j.LoggerFactory

trait WithLog {
  val logg = LoggerFactory.getLogger(this.getClass)
}
