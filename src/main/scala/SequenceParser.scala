import scala.xml.XML

abstract class TagData()
case class TagSequence[T <: TagData](startSentence: Integer, startIndex: Integer, endSentence: Integer, endIndex: Integer, data : T)

/**
 * Created by Keenon on 5/10/14.
 */
abstract class SequenceParser[T <: TagData] {

  case class SentenceIndex(sentence: Integer, index: Integer)

  def parse(path : String) : List[TagSequence[T]] = {
    unwindMentions(SentenceIndex(0,-1),XML.loadFile(path))._1
  }

  def getData(node: xml.Elem): T

  def dataLabel: String

  def unwindMentions(startPos: SentenceIndex, node: xml.Elem) : (List[TagSequence[T]], SentenceIndex) = {

    val recurse = node.child.foldLeft(List[TagSequence[T]](),startPos){

      // Recurse when we find elements that are non-terminal

      case ((corefs, currentPos), child: xml.Elem) => {
        val recurse = unwindMentions(currentPos, child)
        (corefs ++ recurse._1, recurse._2)
      }

      // Consume text input when we reach it, advancing our index appropriately

      case ((corefs, currentPos), child: xml.Node) if child.text == child.toString => {

        // Move the position appropriately when we come to a string

        val tokens : Array[String] = child.toString().split("[ \n]")
        val newPos = tokens.foldLeft(currentPos)((pos,token) => {
          if (List(".","?","!").contains(token.trim)) SentenceIndex(pos.sentence+1,-1) else SentenceIndex(pos.sentence,pos.index+1)
        })

        (corefs, newPos)
      }

      // Catch all other cases, like null node

      case (status, node) => status
    }

    val endPos = recurse._2

    node match {
      case x : xml.Elem if x.label == dataLabel => {
        (TagSequence(startPos.sentence,
            startPos.index,
            endPos.sentence,
            endPos.index-1, // We'll have moved our cursor one step too far, so subtract 1
            getData(x)) :: recurse._1,
          recurse._2)
      }
      case _ => recurse
    }
  }

}
