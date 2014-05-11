import java.io.File

/**
 * Created by Keenon on 5/9/14.
 */
object FileUtils {
  def file(s : String) = new File(s)

  def recursivelyListFiles(f : File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursivelyListFiles)
  }

  def recursivelyListFilePrefixes(f : String): Array[String] = {
    recursivelyListFiles(new File(f)).map(f => f.getAbsolutePath.split(".")(0)).distinct
  }
}

