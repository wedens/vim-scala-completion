package vim.scalacompletion

import scala.reflect.internal.util.Position
import scala.reflect.internal.util.SourceFile

class PositionFactory {
  def create(source: SourceFile, offset: Int) = Position.offset(source, offset)
}
