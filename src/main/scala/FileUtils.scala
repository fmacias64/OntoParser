import java.io.File

/**
 * Created by Keenon on 5/9/14.
 */
object FileUtils {
  def openFile(s : String) = new File(s)

  def recursivelyListFiles(f : File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursivelyListFiles)
  }
}

