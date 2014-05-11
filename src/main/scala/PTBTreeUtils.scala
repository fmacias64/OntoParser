import scala.collection.mutable._

/**
 * Created by Keenon on 5/9/14.
 */
object PTBTreeUtils {

  def getTerminalList(tree: PTBNode, start: Int, length: Int) {
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

  def getAllInterveningTokens(trees : List[PTBNode], startSentence : Int, startIndex : Int, endSentence : Int, endIndex : Int): List[PTBNode] = {
    val sentences = trees.slice(startSentence,endSentence+1)
    sentences.flatMap(s => {
      val terminals = getAllSubtrees(List(s),false)
      val fromIndex : Int = if (trees.indexOf(s) == startSentence) startIndex else 0
      val toIndex : Int = if (trees.indexOf(s) == endSentence) endIndex+1 else terminals.length
      terminals.slice(fromIndex,toIndex)
    })
  }

  def concatTerminals(terminals : List[PTBNode]) : String = {
    terminals.map {
      case t: PTBTerminal => t.token.toString
    }.mkString(" ")
  }

  def specifiedSubtree(tree: PTBNode, mapping: IndexAncestor): PTBNode = {
    specifiedSubtree(tree, mapping.index, mapping.ancestor)
  }

  def specifiedSubtree(tree: PTBNode, startIndex: Int, parents: Int): PTBNode = {
    getParent(getAllSubtrees(List(tree),false)(startIndex),parents)
  }

  def getParent(node: PTBNode, parents: Int): PTBNode = {
    if ((parents > 0) && node.parent != null) getParent(node.parent, parents-1)
    else node
  }

  def flatten(tree: PTBNode): String = {
    tree match {
      case PTBNonTerminal(l,c) => c.map(flatten).mkString(" ")
      case PTBTerminal(l,t) => t
    }
  }

  def cleanFlatten(tree: PTBNode): String = {
    tree match {
      case PTBNonTerminal(l,c) => c.map(cleanFlatten).mkString(" ")
      case PTBTerminal(l,t) => if (t.startsWith("*")) "" else t
    }
  }
}
