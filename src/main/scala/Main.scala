import java.io.{InputStreamReader, FileInputStream}
import scala.util.parsing.input.StreamReader
import com.mongodb.casbah.Imports._

/**
 * Created by Keenon on 5/9/14.
 */
object Main {

  def analyze(list : List[String]) {
    val prepWords : Map[String,Int] = list.map(s =>
      s.split(" ")(0).toLowerCase
    ).foldLeft(Map.empty[String,Int]) {
      (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
    }
    val sortedWords = prepWords.toSeq.sortBy(_._2)

    println("List: \n"+sortedWords.mkString("\n"))
    println("Type-Count: "+sortedWords.length)
    println("Count: "+list.length)
  }

  def main(args: Array[String]) {

    val mongoClient = MongoClient("localhost", 27017)

    val path = "/Users/keenon/Desktop/ontonotes/data/english/annotations"
    OntoParser.parse(FileUtils.recursivelyListFilePrefixes(path)(0))

  }
}

