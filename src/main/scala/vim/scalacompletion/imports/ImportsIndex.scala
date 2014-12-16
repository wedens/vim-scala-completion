package vim.scalacompletion.imports

case class ImportsIndex(sourceIndex: SourceIndex, classFileIndex: ClassFileIndex) {
  def lookupForClass(className: String): Set[String] =
    sourceIndex.lookup(className) ++ classFileIndex.lookup(className)
}
