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

    //val path = "/Users/keenon/Desktop/ontonotes/data/english/annotations/nw/wsj"
    val path = "/Users/keenon/Desktop/ontonotes/data/english/annotations"
    val ptbs : Array[PTBNode] = FileUtils.recursivelyListFiles(FileUtils.openFile(path)).filter(
      f => f.getName.endsWith(".parse")
    ).flatMap(f =>
      {
        val sr = StreamReader(new InputStreamReader(new FileInputStream(f)))
        println("Parsing "+f.getPath)
        PTBParser.parseAll(PTBParser.trees,sr).get
      }
    )

    // val terminals = PTBTreeUtils.getAllSubtrees(ptbs.toList,false).asInstanceOf[List[PTBTerminal]]
    val nonTerminals = PTBTreeUtils.getAllSubtrees(ptbs.toList,true)

    val comparatives : List[String] = nonTerminals.filter(p => List("JJR","JJS","RBR","RBS").contains(p.l)).map(PTBTreeUtils.flatten)
    println("-----------\nCOMPARATIVES\n-----------")
    analyze(comparatives)

    val determiners : List[String] = nonTerminals.filter(p => List("DT").contains(p.l)).map(PTBTreeUtils.flatten)
    println("-----------\nDETERMINERS\n-----------")
    analyze(determiners)

    val modals : List[String] = nonTerminals.filter(p => List("MD").contains(p.l)).map(PTBTreeUtils.flatten)
    println("-----------\nMODALS\n-----------")
    analyze(modals)

    val prepositions : List[String] = nonTerminals.filter(p => List("TO", "IN").contains(p.l)).map(PTBTreeUtils.flatten)
    println("-----------\nPREPOSITIONS\n-----------")
    analyze(prepositions)

    println("Sentences: "+ptbs.length)
  }
}

