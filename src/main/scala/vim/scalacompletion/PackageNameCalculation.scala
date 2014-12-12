package vim.scalacompletion

import java.nio.file.Paths

trait PackageNameCalculation {

  lazy val calculatePackage: Seq[String] => String => Option[String] =
    sourcesDirs => sourcePathStr => {
      val sourcePath = Paths.get(sourcePathStr)
      for {
        sourceDirForPath <- sourcesDirs.find(sourcePath.startsWith)
      } yield {
        val sourcePathRelative = Paths.get(sourceDirForPath).relativize(sourcePath)
        val withoutFileName = sourcePathRelative.getParent
        val pkgName = withoutFileName.toString.replaceAll("/", ".")
        pkgName
      }
    }
}
