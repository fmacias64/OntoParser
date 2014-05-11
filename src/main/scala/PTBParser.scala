
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by Keenon on 5/9/14.
 * PTB Model
 */

abstract class PTBNode(label : String) {
  def contents : String
  def l : String = label
  var parent : PTBNode = null
  override def toString = "("+label+" "+contents+")"
}
case class PTBNonTerminal(label : String, childen : List[PTBNode]) extends PTBNode(label) {
  override def contents = childen.mkString(" ")
}
case class PTBTerminal(label : String, token : String) extends PTBNode(label) {
  override def contents = token
}

/**
 * PTB Parser
 */

object PTBParser extends RegexParsers {
  def token: Parser[String] = "[^ ()]+".r
  def tree: Parser[PTBNode] = "("~>token~(token|trees)<~")" ^^ {
    case (label:String)~(token:String) => PTBTerminal(label,token)
    case (label:String)~(children:List[PTBNode]) => {
      val current = PTBNonTerminal(label,children)
      children.foreach(child => child.parent = current)
      current
    }
  }
  def trees: Parser[List[PTBNode]] = tree.+
}
