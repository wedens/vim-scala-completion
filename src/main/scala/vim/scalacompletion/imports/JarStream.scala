package vim.scalacompletion.imports

import java.io.{FileInputStream, File}
import java.nio.file.Path
import java.util.jar.JarEntry
import java.util.jar.JarInputStream
import scala.util.Try

// TODO: unified error handling during stream processing
object JarStream {
  def apply(path: Path): Stream[JarEntry] = {
    def loop(jis: JarInputStream): Stream[JarEntry] = {
      val entry = jis.getNextJarEntry
      if (entry == null) {
        Try(jis.close)
        Stream.empty
      } else {
        entry #:: loop(jis)
      }
    }
    val jis = new JarInputStream(new FileInputStream(path.toFile))
    loop(jis)
  }
}


