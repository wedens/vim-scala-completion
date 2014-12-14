package vim.scalacompletion

import java.nio.file.{Path, Paths}

trait PackageNameCalculation {
//
//  lazy val calculatePackage: Seq[String] => String => Option[String] =
//    sourcesDirs => sourcePathStr => {
//      val sourcePath = Paths.get(sourcePathStr)
//      for {
//        sourceDirForPath <- sourcesDirs.find(sourcePath.startsWith)
//      } yield {
//        val sourcePathRelative = Paths.get(sourceDirForPath).relativize(sourcePath)
//        val withoutFileName = sourcePathRelative.getParent
//        val pkgName = withoutFileName.toString.replaceAll("/", ".")
//        pkgName
//      }
//    }

  def calculatePackage(sourcesDirs: Set[Path])(file: Path): Option[String] =
    sourcesDirs.find(file.startsWith).map { sourceDirForFile =>
      val sourcePathRelative = sourceDirForFile.relativize(file)
      val withoutFileName = sourcePathRelative.getParent
      val pkgName = withoutFileName.toString.replaceAll("/", ".")
      pkgName
    }
}
