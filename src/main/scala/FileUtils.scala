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

  def recursivelyListFilePrefixes(f : String, suffixes : List[String]): Array[String] = {
    recursivelyListFiles(new File(f)).
      filter(f => f.getAbsolutePath.lastIndexOf(".") != -1).
      filter(f => (suffixes.length == 0) || suffixes.contains(f.getAbsolutePath.substring(f.getAbsolutePath.lastIndexOf(".")+1))).
      map(f => f.getAbsolutePath.substring(0,f.getAbsolutePath.lastIndexOf("."))).
      distinct
  }
}

