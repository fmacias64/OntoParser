import scala.xml.XML

case class CorefMention(sentence: Integer, index: Integer, id: Integer, corefType: String)

/**
 * Created by Keenon on 5/10/14.
 */
object CorefParser {
  def parse(path : String) : List[CorefMention] = {
    val ner = XML.loadFile(path)
    ner.child(1).child.foreach(n => println("Node: "+n))
    List()
  }
}
