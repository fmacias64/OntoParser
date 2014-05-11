import scala.collection.mutable._

/**
 * Created by Keenon on 5/9/14.
 */
object PTBTreeUtils {

  def getTerminalList(tree: PTBNode, start: Integer, length: Integer) {
    getAllSubtrees(List(tree),false).slice(start, start+length)
  }

  def getAllSubtrees(trees : List[PTBNode], includeNonTerminals : Boolean): List[PTBNode] = {
    val list : MutableList[PTBNode] = new MutableList[PTBNode]()
    trees.foreach {
      case tree@PTBNonTerminal(l, c) => {
        list ++= getAllSubtrees(c,includeNonTerminals);
        if (includeNonTerminals) list += tree
      }
      case tree@PTBTerminal(l, t) => list += tree
    }
    list.toList
  }

  def flatten(tree: PTBNode): String = {
    tree match {
      case PTBNonTerminal(l,c) => c.map(flatten).mkString(" ")
      case PTBTerminal(l,t) => t
    }
  }
}
