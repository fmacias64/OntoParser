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
    val sembank = mongoClient("sembank")
    val docbank = sembank("documents")

    // Clear in preparation for new inserts

    MongoInterface.clearDatabase(docbank)

    //println(PropParser.parseAll(PropParser.props,"nw/wsj/00/wsj_0020@0020@wsj@nw@en@on 0 34 gold fail-v fail.01 ----- 34:0-rel 32:1-ARG1 33:1-ARGM-ADV 35:2-ARG2"))
    //println(PropParser.parseAll(PropParser.props,"nw/wsj/00/wsj_0020@0020@wsj@nw@en@on 0 34 gold fail-v fail.01 ----- 34:0-rel 32:1-ARG1 33:1-ARGM-ADV 35:2-ARG2\nnw/wsj/00/wsj_0020@0020@wsj@nw@en@on 0 37 gold honor-v honor.01 ----- 37:0-rel 35:0*32:1-ARG0 38:1-ARG1\nnw/wsj/00/wsj_0020@0020@wsj@nw@en@on 1 17 gold remain-v remain.01 ----- 17:0-rel 0:1-ARGM-DIS 2:2-ARG1 16:0-ARGM-MOD 18:1-ARG3 26:1-ARGM-CAU\nnw/wsj/00/wsj_0020@0020@wsj@nw@en@on 1 39 gold announce-v announce.01 ----- 39:0-rel 34:2-ARG0 40:1-ARG1"))

    val path = "/Users/keenon/Desktop/ontonotes/data/english/annotations"
    val filePrefixes = FileUtils.recursivelyListFilePrefixes(path,List("prop","parse"))
    if (filePrefixes.length > 0) OntoParser.insert(filePrefixes(0),docbank)

    mongoClient.close()

  }
}

