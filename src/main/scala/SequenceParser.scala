import scala.io.Source
import scala.xml.XML

abstract class TagData()
case class TagSequence(startSentence: Int, startIndex: Int, endSentence: Int, endIndex: Int, data : TagData)

/**
 * Created by Keenon on 5/10/14.
 */
abstract class SequenceParser {

  def parse(path : String, trees : List[PTBNode]) : List[TagSequence] = {
    val file = Source.fromFile(path).getLines().map(_.replaceAll(" \" ","")).mkString("\n")
    unwindMentions(new PTBCursor(trees),XML.loadFile(path))
  }

  def getData(node: xml.Elem): TagData

  def dataLabel: String

  def unwindMentions(cursor: PTBCursor, node: xml.Node) : List[TagSequence] = {

    val startPos = cursor.pos

    val recurse = node.child.foldLeft(List[TagSequence]()) {

      // Recurse when we find elements that are non-terminal

      case (corefs, child: xml.Elem) => {
        corefs ++ unwindMentions(cursor, child)
      }

      // Consume text input when we reach it, advancing our index appropriately

      case (corefs, child) => {

        // Move the position appropriately when we come to a string

        child.text.split(" ").foreach(cursor.consumeToken)

        corefs
      }
    }

    val endPos = cursor.pos

    node match {
      case x : xml.Elem if x.label == dataLabel => {
        TagSequence(startPos.sentence,
            if (startPos.index == -1) 0 else startPos.index,
            endPos.sentence,
            endPos.index-1, // We'll have moved our cursor one step too far, so subtract 1
            getData(x)) :: recurse
      }
      case _ => recurse
    }
  }

}
